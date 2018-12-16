#' Coerce any S4 object to a list
#'
#' @inheritParams methods::coerce
#' @inheritParams params
#' @export
#'
#' @seealso `methods::coerce`.
#'
#' @return `list`.
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
    assert(isS4(from))
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



# Consider soft deprecating in favor of `coerceS4ToList`.
flatFiles.SummarizedExperiment <-  # nolint
    function(object) {
        coerceS4ToList(object)
}



#' @rdname coerceS4ToList
#' @export
setMethod(
    f = "flatFiles",
    signature = signature("SummarizedExperiment"),
    definition = flatFiles.SummarizedExperiment
)
