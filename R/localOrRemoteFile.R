#' Dynamically Handle a Local or Remote File Path
#'
#' @importFrom utils download.file
#'
#' @inheritParams AllGenerics
#' @inheritParams saveData
#'
#' @return Named character vector containing the original file name as the
#'   name and local file path as the string. Returns `NULL` on a misisng
#'   local file.
#' @export
localOrRemoteFile <- function(
    object,
    quiet = FALSE) {
    fileName <- basename(object)
    if (grepl("\\://", object)) {
        # Remote file
        filePath <- tempfile()
        download.file(object, filePath, quiet = quiet)
    } else {
        # Local file
        # Check to see if file exists, otherwise return `NULL`
        if (file.exists(object)) {
            filePath <- normalizePath(object)
        } else {
            warn(paste(basename(object), "missing"))
            return(invisible())
        }
    }
    file <- filePath
    names(file) <- fileName
    file
}
