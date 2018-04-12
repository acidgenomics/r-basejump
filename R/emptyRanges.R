#' Generate Empty Genomic Ranges
#'
#' Utility function that provides support for creating internal `GRanges` for
#' transgene and FASTA spike-in sequences.
#'
#' @family Gene Annotation Functions
#' @author Michael Steinbaugh
#'
#' @param names `character` vector of gene or transcript names.
#' @param seqname `character` string that defines the alternative chromosome in
#'   [GenomeInfoDb::seqnames()] where these ranges will be grouped. Defaults to
#'   "`unknown`" but "`transgene`" (transgenes) and "`spike`" (spike-ins) are
#'   also supported.
#' @param mcolsNames *Optional.* `character` vector of metadata column names to
#'   be defined in the [S4Vectors::mcols()] of the `GRanges` return. Normally
#'   this does not need to be defined; useful when combining with another
#'   `GRanges` that contains metadata.
#'
#' @return `GRanges`.
#' @export
#'
#' @seealso
#' - `help("seqinfo", "GenomeInfoDb")`.
#' - `DESeq2::makeExampleDESeqDataSet()`.
#'
#' @examples
#' # Unknown/dead genes
#' emptyRanges("ENSG00000000000", seqname = "unknown")
#'
#' # Transgenes
#' emptyRanges(c("EGFP", "TDTOMATO", "GAL4"), seqname = "transgene")
#'
#' # Spike-ins
#' emptyRanges("ERCC", seqname = "spike")
emptyRanges <- function(
    names,
    seqname = c("unknown", "transgene", "spike"),
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
