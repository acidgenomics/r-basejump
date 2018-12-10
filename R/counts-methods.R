#' Counts
#'
#' Count matrix.
#'
#' @note For a `SummarizedExperiment` object, `"counts"` must be explicitly
#'   defined in `assayNames()`.
#'
#' @name counts
#' @aliases counts<-
#'
#' @inheritParams params
#'
#' @return Matrix. Typically `matrix` or `sparseMatrix` class.
#'
#' @examples
#' data(rse)
#' x <- counts(rse)
#' summary(x)
NULL



#' @importFrom BiocGenerics counts
#' @aliases NULL
#' @export
BiocGenerics::counts

#' @importFrom BiocGenerics counts<-
#' @aliases NULL
#' @export
BiocGenerics::`counts<-`



counts.SummarizedExperiment <-  # nolint
    function(object) {
        validObject(object)
        assertSubset("counts", assayNames(object))
        assays(object)[["counts"]]
    }



#' @rdname counts
#' @export
setMethod(
    f = "counts",
    signature = signature("SummarizedExperiment"),
    definition = counts.SummarizedExperiment
)



`counts<-.SummarizedExperiment` <-  # nolint
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
    definition = `counts<-.SummarizedExperiment`
)
