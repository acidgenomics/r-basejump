#' Match and download a file from a remote directory
#' @export
#' @importFrom RCurl getURL
#' @importFrom stringr str_split str_subset
#' @importFrom utils download.file
#' @keywords web
#' @param remoteDir Remote directory URL
#' @param subset Match \code{string}
#' @param rename Rename the file, if desired
#' @param localDir Directory where to save file locally
downloadFile <- function(remoteDir,
                         string,
                         rename = NULL,
                         localDir = "data-raw") {
    remoteFile <- RCurl::getURL(remoteDir, dirlistonly = TRUE) %>%
        stringr::str_split(., "\n") %>% .[[1]] %>%
        stringr::str_subset(., string)

    # Rename, if desired
    if (!is.null(rename)) {
        localFile <- remoteFile
    } else {
        localFile <- rename
    }

    remoteFile <- paste0(remoteDir, remoteFile)
    localFile <- file.path(localDir, localFile)

    utils::download.file(remoteFile, localFile)
    return(localFile)
}
