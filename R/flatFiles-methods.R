#' Flat Files from S4 Object
#'
#' Extract the slots inside an S4 object for archival storage.
#'
#' @name flatFiles
#' @family Data Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `list` containing the S4 object slots.
#'
#' @examples
#' # SummarizedExperiment ====
#' x <- flatFiles(rse_dds)
#' class(x)
#' names(x)
NULL



# Methods ======================================================================
#' @rdname flatFiles
#' @export
setMethod(
    "flatFiles",
    signature("SummarizedExperiment"),
    function(object) {
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
