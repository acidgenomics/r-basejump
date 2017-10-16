#' Transmit Files from a Remote FTP Server
#'
#' Utility function that supports easy file matching and download from a remote
#' FTP server. Also enables on-the-fly file renaming and compression.
#'
#' @rdname transmit
#' @name transmit
#' @family Data Import and Project Utilities
#'
#' @inheritParams AllGenerics
#' @param object Remote directory URL. Currently supports FTP.
#' @param pattern Pattern to match against remote file names.
#' @param rename Rename the local file (including suffix), if desired.
#' @param compress Compress the file with [gzip()] after download.
#'   (`TRUE`/`FALSE`)
#' @param localDir Directory where to save file locally.
#'
#' @return List of local files.
#' @export
#'
#' @examples
#' transmit("ftp://ftp.ensembl.org/pub/release-90",
#'          pattern = "README",
#'          rename = "ensembl_readme.txt",
#'          compress = TRUE)
NULL



# Constructors ====
#' @importFrom R.utils gzip
#' @importFrom RCurl getURL
#' @importFrom readr read_lines
#' @importFrom stats setNames
#' @importFrom stringr str_extract str_subset
#' @importFrom utils download.file
.transmit <- function(
    object,
    pattern,
    rename = NULL,
    compress = FALSE,
    localDir = "data-raw") {
    remoteDir <- object
    if (!grepl(x = remoteDir, pattern = "ftp\\://")) {
        stop("FTP protocol not detected", call. = FALSE)
    }
    # Fix trailing slash, if necessary
    if (!grepl(x = remoteDir, pattern = "/$")) {
        remoteDir <- paste0(remoteDir, "/")
    }

    remoteList <- remoteDir %>%
        getURL() %>%
        read_lines()
    remoteFileList <- remoteList %>%
        # Match the `-` at begining for file
        # `-rwxrwxr-x`: File
        # `drwxrwxr-x`: Directory
        .[grepl(x = ., pattern = "^-")] %>%
        # File name is at the end, not including a space
        str_extract(pattern = "[^\\s]+$")

    if (!length(remoteFileList)) {
        stop("No files listed on remote server", call. = FALSE)
    }

    # Apply pattern matching
    remoteFileName <- str_subset(remoteFileList, pattern)
    if (!length(remoteFileName)) {
        stop("Pattern didn't match any files", call. = FALSE)
    }

    # Rename files, if desired
    if (!is.null(rename)) {
        if (!identical(length(rename), length(remoteFileName))) {
            stop("Rename vector doesn't match the number of remote files",
                 call. = FALSE)
        }
    }

    # Ensure the local directory exists
    dir.create(localDir, recursive = TRUE, showWarnings = FALSE)
    message(paste("Downloading", toString(remoteFileName)))
    lapply(seq_along(remoteFileName), function(a) {
        # Rename file, if desired
        if (!is.null(rename)) {
            localFileName <- rename[a]
        } else {
            localFileName <- remoteFileName[a]
        }

        remoteFilePath <- paste0(remoteDir, remoteFileName[a])
        localFilePath <- file.path(localDir, localFileName)
        download.file(remoteFilePath, localFilePath)

        # Compress, if desired
        if (isTRUE(compress)) {
            localFilePath <- gzip(localFilePath, overwrite = TRUE)
        }

        localFilePath
    }) %>%
        setNames(remoteFileName)
}



# Methods ====
#' @rdname transmit
#' @export
setMethod(
    "transmit",
    signature("character"),
    .transmit)
