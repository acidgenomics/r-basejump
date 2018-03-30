#' Genomic Ranges from GFF File
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @family GFF Functions
#'
#' @inheritParams general
#' @param format Output as genes or transcripts.
#'
#' @return `GRanges`.
#' @export
#'
#' @examples
#' file <- "http://basejump.seq.cloud/mmusculus.gtf"
#'
#' # Genes
#' x <- makeGRangesFromGFF(file, format = "genes")
#' summary(x)
#'
#' # Transcripts
#' x <- makeGRangesFromGFF(file, format = "transcripts")
#' summary(x)
makeGRangesFromGFF <- function(
    file,
    format = c("genes", "transcripts")
) {
    file <- localOrRemoteFile(file)
    format <- match.arg(format)

    # Attributes ===============================================================
    # Note that GenomicFeatures doesn't support returning the `geneName` from
    # the key value pairs of the GFF file
    attributes <- parseGFFAttributes(
        file,
        select = c("gene_", "transcript_"),
        unique = TRUE
    ) %>%
        as.data.frame() %>%
        camel()
    # Standardize columns into Ensembl format
    colnames(attributes) <- colnames(attributes) %>%
        gsub("^transcript", "tx", .) %>%
        gsub("Symbol$", "Name", .)
    assert_is_subset(
        x = c("txID", "txName", "geneID", "geneName"),
        y = colnames(attributes)
    )

    # GRanges ==================================================================
    # Make TxDb
    txdb <- makeTxDbFromGFF(file)

    # GRanges from TxDb
    if (format == "genes") {
        gr <- genes(txdb, columns = "gene_id")
        colnames(mcols(gr)) <- "geneID"
        attributes <- attributes %>%
            # Merge only the `gene*` columns
            .[, grep("^gene", colnames(.))] %>%
            # Drop rows containing an NA value
            .[complete.cases(.), , drop = FALSE] %>%
            unique()
        assert_has_no_duplicates(attributes[["geneID"]])
        assert_is_subset(
            x = mcols(gr)[["geneID"]],
            y = attributes[["geneID"]]
        )
        merge <- merge(
            x = mcols(gr),
            y = attributes,
            by = "geneID",
            all.x = TRUE
        )
        mcols(gr) <- merge
        assert_are_identical(
            x = names(gr),
            y = mcols(gr)[["geneID"]]
        )
    } else if (format == "transcripts") {
        # `tx_id` returns as integer, so use `tx_name` instead and rename
        gr <- transcripts(txdb, columns = c("tx_name"))
        colnames(mcols(gr)) <- "txID"
        # Need to set the names on the GRanges object manually
        names(gr) <- mcols(gr)[["txID"]]
        # Order GRanges by `txID`
        gr <- gr[sort(names(gr))]
        # Merge the attributes columns
        attributes <- attributes %>%
            # Merge only the `gene*` and `tx*` columns
            .[, grep("^(gene|tx)", colnames(.))] %>%
            # Drop rows containing an NA value
            .[complete.cases(.), , drop = FALSE] %>%
            unique()
        assert_has_no_duplicates(attributes[["txID"]])
        assert_is_subset(
            x = mcols(gr)[["txID"]],
            y = attributes[["txID"]]
        )
        merge <- merge(
            x = mcols(gr),
            y = attributes,
            by = "txID",
            all.x = TRUE
        )
        mcols(gr) <- merge
        assert_are_identical(
            x = names(gr),
            y = mcols(gr)[["txID"]]
        )
    }

    assert_are_identical(names(gr), sort(names(gr)))
    assert_are_identical(names(gr), mcols(gr)[[1L]])

    gr <- .sanitizeAnnotationCols(gr)
    gr <- .addBroadClassCol(gr)

    # Ensure GRanges is sorted by names
    gr <- gr[sort(names(gr))]

    assert_has_names(gr)
    stopifnot(is(gr, "GRanges"))

    gr
}



# Aliases ======================================================================
#' @rdname makeGRangesFromGFF
#' @export
makeGRangesFromGFF -> makeGRangesFromGTF
