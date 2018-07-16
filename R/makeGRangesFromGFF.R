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
    # GRanges from GFF
    gffGn <- gff
    gffGn <- gffGn[!is.na(mcols(gffGn)[["geneID"]])]
    gffGn <- gffGn[grepl("gene", mcols(gffGn)[["type"]])]
    gffGn <- gffGn[!grepl("pseudogene", mcols(gffGn)[["type"]])]
    stopifnot(!any(duplicated(gffGn)))
    assert_has_no_duplicates(mcols(gffGn)[["geneID"]])
    names(gffGn) <- mcols(gffGn)[["geneID"]]
    gffGn <- gffGn[sort(names(gffGn))]
    if (type == "GFF") {
        # geneName
        mcols(gffGn)[["geneName"]] <- mcols(gffGn)[["name"]]
        mcols(gffGn)[["name"]] <- NULL
        # geneBiotype
        mcols(gffGn)[["geneBiotype"]] <- mcols(gffGn)[["biotype"]]
        mcols(gffGn)[["biotype"]] <- NULL
        # Remove extra columns
        mcols(gffGn)[["alias"]] <- NULL
        mcols(gffGn)[["id"]] <- NULL
        mcols(gffGn)[["parent"]] <- NULL
    }

    # GRanges from TxDb
    txdbGn <- genes(txdb)
    txdbGn <- camel(txdbGn)
    assert_is_subset("geneID", colnames(mcols(txdbGn)))
    stopifnot(!any(duplicated(txdbGn)))
    assert_has_no_duplicates(mcols(gffGn)[["geneID"]])
    names(txdbGn) <- mcols(txdbGn)[["geneID"]]
    txdbGn <- txdbGn[sort(names(txdbGn))]

    # Require that GFF and TxDb contain the same ranges
    stopifnot(!length(setdiff(gffGn, txdbGn)))
    assert_are_same_length(gffGn, txdbGn)
    assert_are_identical(
        x = mcols(gffGn)[["geneID"]],
        y = mcols(txdbGn)[["geneID"]]
    )

    message(paste(length(txdbGn), "gene annotations"))
    message(paste(
        "geneID:",
        toString(c(head(names(txdbGn), n = 2L), "..."))
    ))

    if (format == "genes") {
        gr <- gffGn
    }

    # Transcripts ==============================================================
    if (format == "transcripts") {
        # GRanges from TxDb
        txdbTx <- transcripts(txdb)
        txdbTx <- camel(txdbTx)
        colnames(mcols(txdbTx)) <- gsub(
            pattern = "^tx",
            replacement = "transcript",
            x = colnames(mcols(txdbTx))
        )
        # Check for numeric `transcriptID` and replace with `transcriptName`
        assert_is_subset("transcriptID", colnames(mcols(txdbTx)))
        if (type == "GTF") {
            if (is.integer(mcols(txdbTx)[["transcriptID"]])) {
                assert_is_subset("transcriptName", colnames(mcols(txdbTx)))
                mcols(txdbTx)[["transcriptID"]] <-
                    mcols(txdbTx)[["transcriptName"]]
                mcols(txdbTx)[["transcriptName"]] <- NULL
            }
        }
        names(txdbTx) <- mcols(txdbTx)[["transcriptID"]]
        txdbTx <- txdbTx[sort(names(txdbTx))]
        message(paste(length(txdbTx), "transcript annotations"))
        message(paste(
            "transcriptID:",
            toString(c(head(names(txdbTx), n = 2L), "..."))
        ))

        # Attributes from GFF
        if (type == "GTF") {
            mcols <- mcols(gff) %>%
                as.data.frame() %>%
                .[, grepl("^(gene|transcript)", colnames(.)), drop = FALSE] %>%
                filter(!is.na(!!sym("transcriptID"))) %>%
                arrange(!!sym("transcriptID")) %>%
                unique() %>%
                camel() %>%
                as("DataFrame")
            assert_has_no_duplicates(mcols[["transcriptID"]])
            assert_are_identical(
                x = mcols(txdbTx)[["transcriptID"]],
                y = mcols[["transcriptID"]]
            )
            merge <- merge(
                x = mcols(txdbTx),
                y = mcols,
                by = "transcriptID",
                all.x = TRUE
            )
            rownames(merge) <- merge[["transcriptID"]]
            merge <- merge[sort(rownames(merge)), ]
            assert_are_identical(
                x = mcols(txdbTx)[["transcriptID"]],
                y = merge[["transcriptID"]]
            )
            gr <- txdbTx
            mcols(gr) <- merge
        } else if (type == "GFF") {
            gffTx <- gff
            gffTx <- gffTx[!is.na(mcols(gffTx)[["transcriptID"]])]
            # FIXME How to get the right transcripts? mRNA???
            gffTx <- gffTx[grepl("mRNA", mcols(gffTx)[["type"]])]
            # FIXME Not sure how to pick out FlyBase transcripts here
            if (type == "GFF") {
                gffTx <- gffTx[grepl("transcript", mcols(gffTx)[["type"]])]
                assert_has_no_duplicates(mcols(gffTx)[["transcriptID"]])
                names(gffTx) <- mcols(gffTx)[["transcriptID"]]
                # transcriptName
                mcols(gffTx)[["transcriptName"]] <- mcols(gffTx)[["name"]]
                mcols(gffTx)[["name"]] <- NULL
                # transcriptBiotype
                mcols(gffTx)[["transcriptBiotype"]] <- mcols(gffTx)[["biotype"]]
                mcols(gffTx)[["biotype"]] <- NULL
                # geneID
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
            # Merge gene metadata
            geneCols <- setdiff(
                x = colnames(mcols(gffGn)),
                y = colnames(mcols(gffTx))
            )
            geneCols <- c("geneID", geneCols)
            # Need to ensure that `geneID` column is `character` and not
            # `CompressedCharacterList`, otherwise merge will fail here
            mcols <- merge(
                x = mcols(gffTx),
                y = mcols(gffGn)[, geneCols],
                all.x = TRUE,
                by = "geneID"
            )
            rownames(mcols) <- mcols[["transcriptID"]]
            mcols <- mcols[names(gffTx), ]
            assert_are_identical(
                x = mcols(gffTx)[["transcriptID"]],
                y = mcols[["transcriptID"]]
            )
            gr <- gffTx
            mcols(gr) <- mcols
        }
    }

    .makeGRanges(gr)
}



# Aliases ======================================================================
#' @rdname makeGRangesFromGFF
#' @usage NULL
#' @export
makeGRangesFromGFF -> makeGRangesFromGTF
