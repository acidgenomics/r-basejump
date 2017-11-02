#' Dynamically Handle a Local or Remote File Path
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom utils download.file
#'
#' @inheritParams AllGenerics
#' @inheritParams saveData
#'
#' @return Named character vector containing the original file name as the
#'   name and local file path as the string. Returns `NULL` on a misisng
#'   local file.
.localOrRemoteFile <- function(
    object,
    quiet = FALSE) {
    fileName <- basename(object)
    if (grepl(x = object, pattern = "\\://")) {
        # Remote file
        filePath <- tempfile()
        download.file(object, filePath, quiet = quiet)
    } else {
        # Local file
        # Check to see if file exists, otherwise return `NULL`
        if (file.exists(object)) {
            filePath <- normalizePath(object)
        } else {
            warning(paste(basename(object), "missing"), call. = FALSE)
            return(invisible())
        }
    }
    file <- filePath
    names(file) <- fileName
    file
}
