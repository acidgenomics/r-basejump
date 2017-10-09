#' Dynamically Handle a Local or Remote File Path
#'
#' @keywords internal
#' @noRd
#'
#' @inheritParams AllGenerics
#'
#' @return Named character vector containing the original file name as the
#'   name and local file path as the string.
#'
#' @examples
#' \dontrun{
#' file.path(testDataURL, "mtcars.csv") %>%
#'     .localOrRemoteFile()
#' }
.localOrRemoteFile <- function(object) {
    fileName <- basename(object)
    if (str_detect(object, "\\://")) {
        # Remote file
        filePath <- tempfile()
        download.file(object, filePath)
    } else {
        # Local file
        # Check to see if file exists, otherwise return `NULL`
        if (file.exists(object)) {
            filePath <- normalizePath(object)
        } else {
            return(NULL)
        }
    }
    file <- filePath
    names(file) <- fileName
    file
}
