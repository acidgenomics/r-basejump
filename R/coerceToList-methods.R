#' Coerce S4 Object to List
#'
#' Extract the slots inside an S4 object for archival storage.
#'
#' @name coerceToList
#' @family Coercion Methods
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `list`.
#'
#' @examples
#' x <- coerceToList(rse_small)
#' class(x)
#' names(x)
NULL



#' @rdname coerceToList
#' @export
setMethod(
    f = "coerceToList",
    signature = signature("ANY"),
    definition = function(object) {
        stopifnot(isS4(object))
        list <- lapply(slotNames(object), function(slot) {
            if (.hasSlot(object, slot)) {
                slot(object, slot)
            } else {
                NULL  # nocov
            }
        })
        names(list) <- slotNames(object)
        list
    }
)
