#' Counts
#'
#' @name counts
#' @family Data Functions
#'
#' @importFrom BiocGenerics counts
#'
#' @inheritParams general
#'
#' @return `matrix`.
#'
#' @examples
#' # SummarizedExperiment ====
#' x <- counts(rse_dds)
#' summary(x)
NULL



# Methods ======================================================================
#' @rdname counts
#' @export
setMethod(
    "counts",
    "SummarizedExperiment",
    function(object) {
        stopifnot("counts" %in% names(assays(object)))
        assays(object)[["counts"]]
    }
)
