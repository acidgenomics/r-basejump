#' Counts
#'
#' Count matrix.
#'
#' @note For a `SummarizedExperiment` object, `"counts"` must be explicitly
#'   defined in `assayNames`.
#'
#' @name counts
#' @aliases counts<-
#' @note Updated 2019-12-04.
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
#' x <- counts(object)
#' summary(x)
NULL



## Updated 2019-08-06.
`counts,SummarizedExperiment` <-  # nolint
    function(object) {
        validObject(object)
        assay(object, i = "counts")
    }



#' @rdname counts
#' @export
setMethod(
    f = "counts",
    signature = signature("SummarizedExperiment"),
    definition = `counts,SummarizedExperiment`
)



## Updated 2019-08-06.
`counts<-,SummarizedExperiment,matrix` <-  # nolint
    function(object, value) {
        assert(
            all(!is.na(value)),
            all(is.finite(value)),
            all(value >= 0L)
        )
        assay(object, i = "counts") <- value
        validObject(object)
        object
    }



#' @rdname counts
#' @export
setReplaceMethod(
    f = "counts",
    signature = signature(
        object = "SummarizedExperiment",
        value = "matrix"
    ),
    definition = `counts<-,SummarizedExperiment,matrix`
)



## Updated 2019-08-06.
`counts<-,SummarizedExperiment,Matrix` <-  # nolint
    `counts<-,SummarizedExperiment,matrix`



#' @rdname counts
#' @export
setReplaceMethod(
    f = "counts",
    signature = signature(
        object = "SummarizedExperiment",
        value = "Matrix"
    ),
    definition = `counts<-,SummarizedExperiment,Matrix`
)
