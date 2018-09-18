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
            warning("Primary assay is not named `counts`", call. = FALSE)
        }
        assay(object)
    }



#' @rdname counts
#' @export
setMethod(
    f = "counts",
    signature = signature("SummarizedExperiment"),
    definition = .counts.SE
)
