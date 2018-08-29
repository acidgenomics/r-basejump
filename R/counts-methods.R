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



#' @rdname counts
#' @export
setMethod(
    "counts",
    "SummarizedExperiment",
    function(object) {
        object <- .coerceToSummarizedExperiment(object)
        assert_is_subset("counts", assayNames(object))
        assays(object)[["counts"]]
    }
)
