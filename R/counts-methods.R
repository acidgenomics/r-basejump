#' Counts
#'
#' @name counts
#' @family Data Functions
#' @importFrom BiocGenerics counts
#' @export
#'
#' @inheritParams general
#'
#' @return `matrix`.
#'
#' @examples
#' # SummarizedExperiment ====
#' x <- counts(rse_small)
#' summary(x)
NULL



#' @rdname counts
#' @export
setMethod(
    f = "counts",
    "SummarizedExperiment",
    function(object) {
        validObject(object)
        assert_is_subset("counts", assayNames(object))
        assays(object)[["counts"]]
    }
)
