#' Coerce Any S4 Object to List
#'
#' @inheritParams methods::coerce
#' @export
#'
#' @seealso `methods::coerce`.
#'
#' @examples
#' data(rse)
#'
#' x <- coerceS4ToList(rse)
#' class(x)
#' names(x)
#'
#' x <- flatFiles(rse)
#' class(x)
#' names(x)
coerceS4ToList <- function(from) {
    stopifnot(isS4(from))
    to <- lapply(slotNames(from), function(slot) {
        if (.hasSlot(from, slot)) {
            slot(from, slot)
        } else {
            NULL  # nocov
        }
    })
    names(to) <- slotNames(from)
    to
}



# Consider soft deprecating in favor of `coerceS4ToList()`.
flatFiles.SummarizedExperiment <- function(object) {
    coerceS4ToList(object)
}



#' @rdname coerceS4ToList
#' @export
setMethod(
    f = "flatFiles",
    signature = signature("SummarizedExperiment"),
    definition = flatFiles.SummarizedExperiment
)
