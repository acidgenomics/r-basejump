#' Read Log File
#'
#' @rdname readLogFile
#' @name readLogFile
#' @family bcbio Project Directory File Utilities
#' @keywords internal
#'
#' @param object Log file.
#'
#' @return Character vector.
NULL



# Methods ====
#' @rdname readLogFile
#' @importFrom readr read_lines
#' @export
setMethod(
    "readLogFile",
    signature("character"),
    function(object) {
        if (!file.exists(object)) {
            warning(paste(basename(object), "file missing"), call. = FALSE)
            return(NULL)
        }
        read_lines(object)
    })
