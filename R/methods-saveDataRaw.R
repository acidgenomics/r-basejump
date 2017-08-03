#' Save Raw Data
#'
#' Wrapper for [saveData()] that enables quick saving of objects to the
#' `data-raw/` directory.
#'
#' @rdname saveDataRaw
#' @inherit saveData
#'
#' @seealso [devtools::use_data_raw()].
#'
#' @export
setMethod(
    "saveDataRaw",
    signature("..." = "ANY"),
    function(...) {
        saveData(..., dir = "data-raw")
    })
