# TODO Add UCSC support
# TODO Check FlyBase
# TODO Check WormBase
# FIXME Need to figure out how to sanitize geneID from Parent for GFF
# FIXME Always use TxDb to double check that we're returning the right number



#' Genomic Ranges from GFF File
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' The UCSC website has detailed conventions on the GFF3 format, including
#' the metadata columns.
#'
#' @family Gene Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param format Output as genes or transcripts.
#'
#' @return `GRanges`.
#' @export
#'
#' @examples
#' x <- makeGRangesFromGFF("http://basejump.seq.cloud/example.gtf")
#' summary(x)
#' as.data.frame(x) %>% glimpse()
#'
#' # Ensembl GTF genes
#' x <- makeGRangesFromGFF(
#'     file = "~/Mus_musculus.GRCm38.87.gtf.gz",
#'     format = "genes"
#' )
#' colnames(mcols(x))
#'
#' # Ensembl GTF transcripts
#' x <- make
makeGRangesFromGFF <- function(
    file,
    format = c("genes", "transcripts")
) {
    file <- localOrRemoteFile(file)
    # Require GFF or GTF file extension
    stopifnot(grepl("\\.g[ft]f", file, ignore.case = TRUE))
    format <- match.arg(format)

    # Import GFF as GRanges (using rtracklayer)
    gff <- readGFF(file)
    assert_is_all_of(gff, "GRanges")
    gff <- camel(gff)

    # Determine if GFF or GTF
    if (all(c("id", "name") %in% colnames(mcols(gff)))) {
        type <- "GFF"
    } else {
        type <- "GTF"
    }

    # Report the source of the gene annotations
    assert_is_subset("source", colnames(mcols(gff)))
    if (any(grepl("FlyBase", mcols(gff)[["source"]]))) {
        source <- "FlyBase"
    } else if (any(grepl("WormBase", mcols(gff)[["source"]]))) {
        source <- "WormBase"
    } else if (any(grepl(
        "ensembl", mcols(gff)[["source"]], ignore.case = TRUE
    ))) {
        source <- "Ensembl"
    } else {
        stop("Unsupported GFF source")
    }

    message(paste(source, type, "detected"))
    if (type == "GFF") {
        warning("Input of GTF (GFFv2) is preferred over GFF3, if possible")
    }

    # Always require `geneID` and `transcriptID` columns in file
    assert_is_subset(
        x = c("geneID", "transcriptID"),
        y = colnames(mcols(gff))
    )

    # FlyBase: Rename `symbol` to `name`
    if (source == "FlyBase") {
        message("Renaming `*Symbol` to `*Name`")
        colnames(mcols(gff)) <- gsub(
            pattern = "Symbol$",
            replacement = "Name",
            x = colnames(mcols(gff))
        )
    }

    # Transcript database (GenomicFeatures TxDb)
    txdb <- suppressWarnings(makeTxDbFromGFF(file))

    # Genes ====================================================================
    # GRanges from TxDb
    txdbGn <- genes(txdb)

    # GRanges from GFF
    gffGn <- gff
    gffGn <- gffGn[!is.na(mcols(gffGn)[["geneID"]])]
    gffGn <- gffGn[grepl("gene", mcols(gffGn)[["type"]])]
    # FIXME This will error out with FlyBase? Keep this line?
    # gffGn <- gffGn[!grepl("pseudogene", mcols(gffGn)[["type"]])]
    stopifnot(!any(duplicated(gffGn)))
    assert_has_no_duplicates(mcols(gffGn)[["geneID"]])
    names(gffGn) <- mcols(gffGn)[["geneID"]]
    gffGn <- gffGn[sort(names(gffGn))]
    if (type == "GFF") {
        # geneName
        assert_is_subset("name", colnames(mcols(gffGn)))
        mcols(gffGn)[["geneName"]] <- mcols(gffGn)[["name"]]
        mcols(gffGn)[["name"]] <- NULL
        # geneBiotype
        assert_is_subset("biotype", colnames(mcols(gffGn)))
        mcols(gffGn)[["geneBiotype"]] <- mcols(gffGn)[["biotype"]]
        mcols(gffGn)[["biotype"]] <- NULL
        # Remove extra columns
        mcols(gffGn)[["alias"]] <- NULL
        mcols(gffGn)[["id"]] <- NULL
        mcols(gffGn)[["parent"]] <- NULL
    }

    # Intersection of GFF and TxDb
    gn <- gffGn[gffGn %in% txdbGn]
    assert_has_no_duplicates(mcols(gn)[["geneID"]])
    names(gn) <- mcols(gn)[["geneID"]]
    gn <- gn[sort(names(gn))]

    message(paste(length(gn), "gene annotations"))
    message(paste(
        "geneID:",
        toString(c(head(names(gn), n = 2L), "..."))
    ))

    if (format == "genes") {
        gr <- gn
    }

    # Transcripts ==============================================================
    if (format == "transcripts") {
        # GRanges from TxDb
        txdbTx <- transcripts(txdb)

        # GRanges from GFF
        gffTx <- gff[!is.na(mcols(gff)[["transcriptID"]])]
        if (type == "GFF") {
            # transcriptName
            assert_is_subset("name", colnames(mcols(gffTx)))
            mcols(gffTx)[["transcriptName"]] <- mcols(gffTx)[["name"]]
            mcols(gffTx)[["name"]] <- NULL
            # transcriptBiotype
            assert_is_subset("biotype", colnames(mcols(gffTx)))
            mcols(gffTx)[["transcriptBiotype"]] <- mcols(gffTx)[["biotype"]]
            mcols(gffTx)[["biotype"]] <- NULL
            # geneID
            assert_is_subset("parent", colnames(mcols(gffTx)))
            stopifnot(all(grepl("^gene:", mcols(gffTx)[["parent"]])))
            mcols(gffTx)[["geneID"]] <- as.character(mcols(gffTx)[["parent"]])
            mcols(gffTx)[["geneID"]] <- gsub(
                pattern = "^gene:",
                replacement = "",
                x = mcols(gffTx)[["geneID"]]
            )
            # Remove extra columns
            mcols(gffTx)[["alias"]] <- NULL
            mcols(gffTx)[["id"]] <- NULL
            mcols(gffTx)[["parent"]] <- NULL
        }

        # Intersection of GFF and TxDb
        tx <- gffTx[gffTx %in% txdbTx]
        # FIXME This errors out
        # FIXME Better way to handle dupes?
        assert_has_no_duplicates(mcols(tx)[["transcriptID"]])
        names(tx) <- mcols(tx)[["transcriptID"]]
        tx <- tx[sort(names(tx))]

        message(paste(length(tx), "transcript annotations"))
        message(paste(
            "transcriptID:",
            toString(c(head(names(tx), n = 2L), "..."))
        ))

        # Merge the gene-level annotations (`geneName`, `geneBiotype`)
        geneCols <- setdiff(colnames(mcols(gn)), colnames(mcols(tx)))
        geneCols <- c("geneID", geneCols)
        merge <- merge(
            x = mcols(tx),
            y = mcols(gn)[, geneCols],
            all.x = TRUE,
            by = "geneID"
        )
        rownames(merge) <- merge[["transcriptID"]]
        merge <- merge[sort(rownames(merge)), ]
        assert_are_identical(
            x = mcols(tx)[["transcriptID"]],
            y = merge[["transcriptID"]]
        )

        gr <- tx
        mcols(gr) <- merge
    }

    # Warn if any identifiers are dropped
    if (format == "genes") {
        ids <- mcols(gff)[["geneID"]]
    } else if (format == "transcripts") {
        ids <- mcols(gff)[["transcriptID"]]
    }
    ids <- sort(unique(na.omit(ids)))
    assert_are_identical(ids, names(gr))

    .makeGRanges(gr)
}



# Aliases ======================================================================
#' @rdname makeGRangesFromGFF
#' @usage NULL
#' @export
makeGRangesFromGFF -> makeGRangesFromGTF
