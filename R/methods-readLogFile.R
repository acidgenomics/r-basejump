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
#' url <- file.path(
#'     "http://basejump.seq.cloud",
#'     "bcbio",
#'     "bcbio-nextgen.log")
#' readLogFile(url) %>% head()
NULL



# Methods ======================================================================
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
            return(invisible())
        }
        read_lines(file)
    })
