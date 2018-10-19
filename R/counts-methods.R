#' @importFrom BiocGenerics counts
#' @aliases NULL
#' @export
BiocGenerics::counts

#' @importFrom BiocGenerics counts<-
#' @aliases NULL
#' @export
BiocGenerics::`counts<-`



#' Counts
#'
#' Count matrix.
#'
#' @note For a `SummarizedExperiment` object, `"counts"` must be explicitly
#'   defined in [assayNames()].
#'
#' @name counts
#' @family SummarizedExperiment Functions
#'
#' @inheritParams general
#'
#' @return Matrix. Typically `matrix` or `sparseMatrix` class.
#'
#' @examples
#' data(rse_small)
#' x <- counts(rse_small)
#' summary(x)
NULL



.counts.SummarizedExperiment <-  # nolint
    function(object) {
        validObject(object)
        assert_is_subset("counts", assayNames(object))
        assays(object)[["counts"]]
    }



`.counts<-.SummarizedExperiment` <-  # nolint
    function(object, value) {
        validObject(object)
        assays(object)[["counts"]] <- value
        object
    }



#' @rdname counts
#' @export
setMethod(
    f = "counts",
    signature = signature("SummarizedExperiment"),
    definition = .counts.SummarizedExperiment
)



#' @rdname counts
#' @export
setMethod(
    f = "counts<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "ANY"
    ),
    definition = `.counts<-.SummarizedExperiment`
)



#' @rdname counts
#' @export
setMethod(
    f = "counts<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "matrix"
    ),
    definition = `.counts<-.SummarizedExperiment`
)
