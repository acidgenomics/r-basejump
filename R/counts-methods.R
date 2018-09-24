#' Counts
#'
#' @note For a `SummarizedExperiment` object, `"counts"` must be explicitly
#'   defined in [assayNames()].
#'
#' @name counts
#' @family Data Functions
#' @importFrom BiocGenerics counts
#' @export
#'
#' @inheritParams general
#'
#' @return Counts matrix. Typically `matrix` or `sparseMatrix` class.
#'
#' @examples
#' # SummarizedExperiment ====
#' x <- counts(rse_small)
#' summary(x)
NULL



#' @rdname counts
#' @name counts<-
#' @importFrom BiocGenerics counts<-
#' @export
NULL



.counts.SE <-  # nolint
    function(object) {
        validObject(object)
        assert_is_subset("counts", assayNames(object))
        assays(object)[["counts"]]
    }



`.counts<-.SE` <-  # nolint
    function(object, value) {
        validObject(object)
        assert_is_subset("counts", assayNames(object))
        assays(object)[["counts"]] <- value
        object
    }



#' @rdname counts
#' @export
setMethod(
    f = "counts",
    signature = signature("SummarizedExperiment"),
    definition = .counts.SE
)



#' @rdname counts
#' @export
setMethod(
    f = "counts<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "ANY"
    ),
    definition = `.counts<-.SE`
)



#' @rdname counts
#' @export
setMethod(
    f = "counts<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "matrix"
    ),
    definition = getMethod(
        f = "counts<-",
        signature(
            object = "SummarizedExperiment",
            value = "ANY"
        )
    )
)



#' @rdname counts
#' @export
setMethod(
    f = "counts<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "sparseMatrix"
    ),
    definition = getMethod(
        f = "counts<-",
        signature(
            object = "SummarizedExperiment",
            value = "ANY"
        )
    )
)
