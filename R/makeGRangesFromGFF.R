# TODO Check FlyBase
# TODO Check WormBase
# TODO Check UCSC?
# FIXME Need to figure out how to sanitize geneID from Parent for GFF



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
#' makeGRangesFromGFF(
#'     file = "~/Mus_musculus.GRCm38.87.gtf.gz"
#' )
makeGRangesFromGFF <- function(
    file,
    format = c("genes", "transcripts")
) {
    file <- localOrRemoteFile(file)
    # Require GFF or GTF file extension
    stopifnot(grepl("\\.g[ft]f", file, ignore.case = TRUE))
    format <- match.arg(format)

    # Import GFF as GRanges
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

    # Genes (gn; always generate)
    gn <- gff
    gn <- gn[!is.na(mcols(gn)[["geneID"]])]
    gn <- gn[grepl("gene", mcols(gn)[["type"]])]
    # Drop pseudogene rows (e.g. FlyBase GTF)
    gn <- gn[!grepl("pseudogene", mcols(gn)[["type"]])]
    assert_has_no_duplicates(mcols(gn)[["geneID"]])
    names(gn) <- mcols(gn)[["geneID"]]
    if (type == "GFF") {
        # geneName
        mcols(gn)[["geneName"]] <- mcols(gn)[["name"]]
        mcols(gn)[["name"]] <- NULL
        # geneBiotype
        mcols(gn)[["geneBiotype"]] <- mcols(gn)[["biotype"]]
        mcols(gn)[["biotype"]] <- NULL
        # Remove extra columns
        mcols(gn)[["alias"]] <- NULL
        mcols(gn)[["id"]] <- NULL
        mcols(gn)[["parent"]] <- NULL
    }

    if (format == "genes") {
        gr <- gn
    } else if (format == "transcripts") {
        if (type == "GTF") {
            txdb <- suppressWarnings(makeTxDbFromGFF(file))
            tx <- transcripts(txdb)
            tx <- camel(tx)
            mcols(tx)[["txID"]] <- mcols(tx)[["txName"]]
            mcols(tx)[["txName"]] <- NULL
        } else if (type == "GFF") {

        }

        # Transcripts (tx)
        tx <- gff
        tx <- tx[!is.na(mcols(tx)[["transcriptID"]])]
        # FIXME How to get the right transcripts? mRNA???
        tx <- tx[grepl("mRNA", mcols(tx)[["type"]])]
        # FIXME Not sure how to pick out FlyBase transcripts here
        if (type == "GFF") {
            tx <- tx[grepl("transcript", mcols(tx)[["type"]])]
            assert_has_no_duplicates(mcols(tx)[["transcriptID"]])
            names(tx) <- mcols(tx)[["transcriptID"]]
            # transcriptName
            mcols(tx)[["transcriptName"]] <- mcols(tx)[["name"]]
            mcols(tx)[["name"]] <- NULL
            # transcriptBiotype
            mcols(tx)[["transcriptBiotype"]] <- mcols(tx)[["biotype"]]
            mcols(tx)[["biotype"]] <- NULL
            # geneID
            stopifnot(all(grepl("^gene:", mcols(tx)[["parent"]])))
            mcols(tx)[["geneID"]] <- as.character(mcols(tx)[["parent"]])
            mcols(tx)[["geneID"]] <- gsub(
                pattern = "^gene:",
                replacement = "",
                x = mcols(tx)[["geneID"]]
            )
            # Remove extra columns
            mcols(tx)[["alias"]] <- NULL
            mcols(tx)[["id"]] <- NULL
            mcols(tx)[["parent"]] <- NULL
        }
        # Merge gene metadata
        geneCols <- setdiff(
            x = colnames(mcols(gn)),
            y = colnames(mcols(tx))
        )
        geneCols <- c("geneID", geneCols)
        # Need to ensure that `geneID` column is `character` and not
        # `CompressedCharacterList`, otherwise merge will fail here
        mcols <- merge(
            x = mcols(tx),
            y = mcols(gn)[, geneCols],
            all.x = TRUE,
            by = "geneID"
        )
        rownames(mcols) <- mcols[["transcriptID"]]
        mcols <- mcols[names(tx), ]
        assert_are_identical(
            x = mcols(tx)[["transcriptID"]],
            y = mcols[["transcriptID"]]
        )
        gr <- tx
        mcols(gr) <- mcols
    }

    .makeGRanges(gr)
}



# Aliases ======================================================================
#' @rdname makeGRangesFromGFF
#' @usage NULL
#' @export
makeGRangesFromGFF -> makeGRangesFromGTF
