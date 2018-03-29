#' Define Row Annotations from GFF/GTF File
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @family GFF Functions
#'
#' @inheritParams general
#' @param level Choose "`genes` or "`transcripts`".
#'
#' @return `data.frame`.
#' @export
#'
#' @examples
#' file <- "http://basejump.seq.cloud/mmusculus.gtf"
#'
#' # Genes
#' x <- rowRangesFromGFF(file, level = "genes")
#' head(names(x))
#' head(mcols(x))
#'
#' # Transcripts
#' x <- rowRangesFromGFF(file, level = "transcripts")
#' head(names(x))
#' head(mcols(x))
rowRangesFromGFF <- function(
    file,
    level = c("genes", "transcripts")
) {
    file <- localOrRemoteFile(file)
    level <- match.arg(level)

    # Prepare TxDb
    # Warnings are common at this step
    txdb <- suppressWarnings(makeTxDbFromGFF(file))

    # Note that GenomicFeatures doesn't support returning the `geneName` from
    # the key value pairs of the GFF file
    gff <- suppressMessages(readGFF(file))
    extra <- .gffKeyValuePairs(gff, unique = TRUE)

    # Standardize columns into Ensembl format
    colnames(extra) <- colnames(extra) %>%
        gsub("^transcript", "tx", .) %>%
        gsub("Symbol$", "Name", .)
    requiredCols <- c(
        "txID",
        "txName",
        "txBiotype",
        "geneID",
        "geneName",
        "geneBiotype"
    )
    assert_is_subset(requiredCols, colnames(extra))

    if (level == "genes") {
        gr <- genes(txdb, columns = "gene_id")
        colnames(mcols(gr)) <- "geneID"
        extra <- extra %>%
            # Merge only the `gene*` columns
            .[, grep("^gene", colnames(.))] %>%
            # Drop rows containing an NA value
            filter_all(all_vars(is_not_na(.))) %>%
            unique() %>%
            as("DataFrame")
        assert_has_no_duplicates(extra[["geneID"]])
        merge <- merge(
            x = mcols(gr),
            y = extra,
            by = "geneID",
            all.x = TRUE
        )
        mcols(gr) <- merge
        assert_are_identical(
            x = names(gr),
            y = mcols(gr)[["geneID"]]
        )
    } else if (level == "transcripts") {
        # `tx_id` returns as integer, so use `tx_name` instead and rename
        gr <- transcripts(txdb, columns = c("tx_name"))
        colnames(mcols(gr)) <- "txID"
        # Need to set the names on the GRanges object manually
        names(gr) <- mcols(gr)[["txID"]]
        # Order GRanges by `txID`
        gr <- gr[sort(names(gr))]
        # Merge the extra columns
        extra <- extra %>%
            # Merge only the `gene*` and `tx*` columns
            .[, grep("^(gene|tx)", colnames(.))] %>%
            # Drop rows containing an NA value
            filter_all(all_vars(is_not_na(.))) %>%
            unique() %>%
            as("DataFrame")
        assert_has_no_duplicates(extra[["txID"]])
        merge <- merge(
            x = mcols(gr),
            y = extra,
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

    # Add the broad class column
    gr <- .addBroadClassCol(gr)

    gr
}



# Aliases ======================================================================
#' @rdname rowRangesFromGFF
#' @export
rowRangesFromGFF -> rowRangesFromGTF
