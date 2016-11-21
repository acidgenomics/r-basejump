#' Find and download a file from a remote server.
#'
#' @import RCurl
#' @import stringr
#' @importFrom utils download.file
#'
#' @param url URL of remote directory.
#' @param subset Match string.
#' @param rename Rename the file, if desired.
#' @param data_raw Append \code{data-raw} to saved file name.

#' @export
downloadFile <- function(url, subset, rename = NULL, data_raw = TRUE) {
  remoteFile <- RCurl::getURL(url, dirlistonly = TRUE) %>%
    stringr::str_split(., "\n") %>%
    .[[1]] %>%
    stringr::str_subset(., subset)
  if (is.null(rename)) {
    filePath <- remoteFile
  } else {
    filePath <- rename
  }
  if (isTRUE(data_raw)) {
    if (!file.exists("data-raw")) {
      dir.create("data-raw")
    }
    filePath <- file.path("data-raw", filePath)
  }
  if (!file.exists(filePath)) {
    utils::download.file(paste0(url, remoteFile), filePath)
  }
  return(filePath)
}
