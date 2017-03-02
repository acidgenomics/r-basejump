#' Match and download a file from a remote directory
#' @export
#' @importFrom RCurl getURL
#' @importFrom R.utils gzip
#' @importFrom stringr str_split str_subset
#' @importFrom utils download.file
#' @keywords web
#' @param remoteDir Remote directory URL
#' @param string \code{string} to match against remote file names
#' @param rename Rename the local file, if desired
#' @param compress Whether to compress the file (gzip) after download
#' @param localDir Directory where to save file locally
downloadFile <- function(remoteDir,
                         string,
                         rename = NULL,
                         compress = FALSE,
                         localDir = "data-raw") {
    # Create the local directory, if necessary
    if (!is.null(localDir)) {
        if (!dir.exists(localDir)) {
            dir.create(localDir, recursive = TRUE)
        }
    } else {
        stop("Local directory cannot be NULL.")
    }

    remoteFile <- RCurl::getURL(remoteDir, dirlistonly = TRUE) %>%
        stringr::str_split(., "\n") %>% .[[1]] %>%
        stringr::str_subset(., string)

    # Rename, if desired
    if (!is.null(rename)) {
        localFile <- rename
    } else {
        localFile <- remoteFile
    }

    remoteFile <- paste0(remoteDir, remoteFile)
    localFile <- file.path(localDir, localFile)

    utils::download.file(remoteFile, localFile)

    # Compress, if desired
    if (isTRUE(compress)) {
        R.utils::gzip(localFile, overwrite = TRUE)
        localFile <- paste0(localFile, ".gz")
    }

    return(localFile)
}
