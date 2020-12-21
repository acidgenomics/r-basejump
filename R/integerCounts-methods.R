#' @name integerCounts
#' @inherit AcidGenerics::integerCounts
#' @note Updated 2020-12-21.
#'
#' @note For a `SummarizedExperiment` object, `"counts"` must be explicitly
#'   defined in `assayNames`.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return Matrix.
#' Typically `matrix` or `Matrix` class.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' x <- integerCounts(object)
#' class(x)
#' is.integer(x)
#' summary(x)
NULL



## This is inspired by approach used internally by DESeq2.
## Updated 2019-12-04.
`integerCounts,matrix` <-  # nolint
    function(object) {
        object <- round(x = object, digits = 0L)
        mode(object) <- "integer"
        object
    }



#' @rdname integerCounts
#' @export
setMethod(
    f = "integerCounts",
    signature = signature("matrix"),
    definition = `integerCounts,matrix`
)



## Updated 2019-12-04.
`integerCounts,Matrix` <-  # nolint
    function(object) {
        object <- round(x = object, digits = 0L)
        object
    }



#' @rdname integerCounts
#' @export
setMethod(
    f = "integerCounts",
    signature = signature("Matrix"),
    definition = `integerCounts,Matrix`
)



## Updated 2019-12-04.
`integerCounts,SummarizedExperiment` <-  # nolint
    function(object) {
        integerCounts(counts(object))
    }



#' @rdname integerCounts
#' @export
setMethod(
    f = "integerCounts",
    signature = signature("SummarizedExperiment"),
    definition = `integerCounts,SummarizedExperiment`
)
