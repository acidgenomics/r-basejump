#' Counts
#'
#' Count matrix.
#'
#' @note For a `SummarizedExperiment` object, `"counts"` must be explicitly
#'   defined in `assayNames`.
#'
#' @name counts
#' @aliases counts<-
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return Matrix.
#' Typically `matrix` or `sparseMatrix` class.
#'
#' @examples
#' data(rse, package = "acidtest")
#' x <- counts(rse)
#' summary(x)
NULL



counts.SummarizedExperiment <-  # nolint
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
    definition = counts.SummarizedExperiment
)



`counts<-.SummarizedExperiment,ANY` <-  # nolint
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
    definition = `counts<-.SummarizedExperiment,ANY`
)
