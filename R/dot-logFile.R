#' Read Log File
#'
#' @family bcbio Project Directory File Utilities
#' @keywords internal
#'
#' @param file Log file.
#'
#' @return Character vector.
#' @export
.logFile <- function(file) {
    if (!file.exists(file)) {
        warning(paste(basename(file), "missing"))
        return(NULL)
    }
    read_lines(file)
}
