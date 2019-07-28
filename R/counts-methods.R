#' Counts
#'
#' Count matrix.
#'
#' @note For a `SummarizedExperiment` object, `"counts"` must be explicitly
#'   defined in `assayNames`.
#'
#' @name counts
#' @aliases counts<-
#' @note Updated 2019-07-28.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return Matrix.
#' Typically `matrix` or `sparseMatrix` class.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' x <- counts(rse)
#' summary(x)
NULL



#' @rdname counts
#' @name counts
#' @importFrom BiocGenerics counts
#' @usage counts(object, ...)
#' @export
NULL

#' @rdname counts
#' @name counts<-
#' @importFrom BiocGenerics counts<-
#' @usage counts(object, ...) <- value
#' @export
NULL



## Updated 2019-07-22.
`counts,SummarizedExperiment` <-  # nolint
    function(object) {
        validObject(object)
        assert(isSubset("counts", assayNames(object)))
        assays(object)[["counts"]]
    }



#' @rdname counts
#' @export
setMethod(
    f = "counts",
    signature = signature("SummarizedExperiment"),
    definition = `counts,SummarizedExperiment`
)



## Updated 2019-07-22.
`counts<-,SummarizedExperiment,ANY` <-  # nolint
    function(object, value) {
        validObject(object)
        assays(object)[["counts"]] <- value
        object
    }



#' @rdname counts
#' @export
setMethod(
    f = "counts<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "ANY"
    ),
    definition = `counts<-,SummarizedExperiment,ANY`
)
