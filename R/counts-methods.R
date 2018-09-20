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



.counts.SE <-  # nolint
    function(object) {
        validObject(object)
        if (!identical("counts", assayNames(object)[[1L]])) {
            stop("Primary assay is not named `counts`", call. = FALSE)
        }
        assays(object)[["counts"]]
    }



`.counts<-.SE` <-  # nolint
    function(object, value) {
        validObject(object)
        if (!identical("counts", assayNames(object)[[1L]])) {
            stop("Primary assay is not named `counts`", call. = FALSE)
        }
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



#' @rdname sampleData
#' @export
setMethod(
    f = "counts<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "ANY"
    ),
    definition = `.counts<-.SE`
)
