#' Generate Empty Genomic Ranges
#'
#' Utility function that provides support for creating internal `GRanges` for
#' transgene and FASTA spike-in sequences.
#'
#' @family Gene Annotation Functions
#' @author Michael Steinbaugh
#'
#' @return `GRanges`.
#' @export
#'
#' @seealso
#' - `help("seqinfo", "GenomeInfoDb")`.
#' - `DESeq2::makeExampleDESeqDataSet()`.
#'
#' @examples
#' emptyRanges(c("EGFP", "TDTOMATO", "GAL4"), seqname = "transgene")
emptyRanges <- function(
    names,
    seqname = "unknown",
    mcolsNames = NULL
) {
    assert_is_character(names)
    assert_is_a_string(seqname)
    assert_is_any_of(mcolsNames, c("character", "NULL"))

    gr <- GRanges(
        seqnames = seqname,
        ranges = IRanges(
            start = (1L:length(names) - 1L) * 100L + 1L,
            width = 100L
        )
    )
    names(gr) <- names

    # Create the required empty metadata columns
    if (!length(mcolsNames)) {
        ncol <- 0L
    } else {
        ncol <- length(mcolsNames)
    }
    mcols <- matrix(
        nrow = length(names),
        ncol = ncol,
        dimnames = list(
            names,
            mcolsNames
        )
    )
    mcols <- as(mcols, "DataFrame")
    mcols(gr) <- mcols

    gr
}
