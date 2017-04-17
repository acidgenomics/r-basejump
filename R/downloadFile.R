#' Match and download a file from a remote directory
#'
#' @author Michael Steinbaugh
#'
#' @param remoteDir Remote directory URL
#' @param string String to match against remote file names
#' @param rename Rename the local file, if desired
#' @param compress Whether to compress the file (\code{gzip}) after download
#' @param localDir Directory where to save file locally
#'
#' @return Local file name
#' @export
downloadFile <- function(
    remoteDir,
    string,
    rename = NULL,
    compress = FALSE,
    localDir = "data-raw") {
    dir.create(localDir, recursive = TRUE, showWarnings = FALSE)

    remoteFile <- getURL(remoteDir, dirlistonly = TRUE) %>%
        str_split("\n") %>% .[[1]] %>%
        str_subset(string)

    # Rename, if desired
    if (!is.null(rename)) {
        localFile <- rename
    } else {
        localFile <- remoteFile
    }

    remoteFile <- paste0(remoteDir, remoteFile)
    localFile <- file.path(localDir, localFile)

    download.file(remoteFile, localFile)

    # Compress, if desired
    if (isTRUE(compress)) {
        gzip(localFile, overwrite = TRUE)
        localFile <- paste0(localFile, ".gz")
    }

    return(localFile)
}
