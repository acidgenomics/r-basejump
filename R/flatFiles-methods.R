#' Flat Files from S4 Object
#'
#' Extract the slots inside an S4 object for archival storage.
#'
#' @name flatFiles
#' @family Developer Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `list`.
#'
#' @examples
#' x <- flatFiles(rse_small)
#' class(x)
#' names(x)
NULL



#' @rdname flatFiles
#' @export
setMethod(
    f = "flatFiles",
    signature = signature("ANY"),
    definition = function(object) {
        # Return unmodified if not S4.
        if (!isS4(object)) {
            return(object)
        }
        validObject(object)
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
