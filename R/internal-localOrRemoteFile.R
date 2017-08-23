#' Dynamically Handle a Local or Remote File Path
#'
#' @rdname internal-localOrRemoteFile
#'
#' @inheritParams AllGenerics
#'
#' @return Named character vector containing the original file name as the
#'   name and local file path as the string.
#'
#' @examples
#' # Remote
#' file.path(testDataURL, "mtcars.csv") %>%
#'     .localOrRemoteFile
.localOrRemoteFile <- function(object) {
    fileName <- basename(object)
    if (str_detect(object, "\\://")) {
        # Remote file
        filePath <- tempfile()
        download.file(object, filePath)
    } else {
        # Local file
        filePath <- normalizePath(object)
    }
    file <- filePath
    names(file) <- fileName
    file
}
