#' Read Log File
#'
#' @rdname readLogFile
#' @name readLogFile
#' @family bcbio Project Directory File Utilities
#' @keywords internal
#'
#' @inheritParams saveData
#'
#' @param object Log file.
#'
#' @return Character vector.
#'
#' @examples
#' readLogFile(file.path(testDataURL, "bcbio-nextgen.log")) %>% head()
NULL



# Methods ====
#' @rdname readLogFile
#' @importFrom readr read_lines
#' @export
setMethod(
    "readLogFile",
    signature("character"),
    function(
        object,
        quiet = FALSE) {
        file <- .localOrRemoteFile(object, quiet = quiet)
        if (is.null(file)) {
            return(NULL)
        }
        read_lines(file)
    })
