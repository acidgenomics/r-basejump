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
#' @seealso `help("seqinfo", "GenomeInfoDb")`.
#'
#' @examples
#' emptyRanges("EGFP", seqname = "transgene")
emptyRanges <- function(
    names,
    seqname = "transgene",
    mcolsNames = NULL
) {
    assert_is_character(names)
    assert_is_a_string(seqname)
    assert_is_any_of(mcolsNames, c("character", "NULL"))

    x <- paste(seqname, "1-100", sep = ":")
    x <- replicate(n = length(names), expr = x)

    gr <- GRanges(x)
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
