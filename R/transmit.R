#' Transmit Files from a Remote FTP Server
#'
#' Utility function that supports easy file matching and download from a remote
#' FTP server. Also enables on-the-fly file renaming and compression.
#'
#' @family Data Import and Project Utilities
#'
#' @importFrom R.utils gzip
#' @importFrom RCurl getURL
#' @importFrom readr read_lines
#' @importFrom stats setNames
#' @importFrom stringr str_extract str_subset
#' @importFrom utils download.file
#'
#' @inheritParams AllGenerics
#' @inheritParams saveData
#'
#' @param remoteDir Remote directory URL. Currently supports FTP. Works either
#'   with or without the trailing slash.
#' @param localDir Directory where to save file locally.
#' @param pattern Pattern to match against remote file names.
#' @param rename Rename the local file (including suffix), if desired.
#' @param compress Compress the file with [gzip()] after download.
#'   (`TRUE`/`FALSE`)
#'
#' @return Invisibly return a list of the files downloaded.
#' @export
#'
#' @examples
#' transmit(
#'     remoteDir = "ftp://ftp.ensembl.org/pub/release-90",
#'     pattern = "README",
#'     rename = "ensembl_readme.txt",
#'     compress = TRUE)
#'
#' # Clean up
#' unlink("ensembl_readme.txt.gz")
transmit <- function(
    remoteDir,
    localDir = getwd(),
    pattern,
    rename = NULL,
    compress = FALSE,
    quiet = FALSE) {
    if (!grepl("ftp\\://", remoteDir)) {
        abort("FTP protocol not detected")
    }
    # Fix trailing slash, if necessary
    if (!grepl("/$", remoteDir)) {
        remoteDir <- paste0(remoteDir, "/")
    }

    remoteList <- remoteDir %>%
        getURL() %>%
        read_lines()
    remoteFileList <- remoteList %>%
        # Match the `-` at begining for file
        # `-rwxrwxr-x`: File
        # `drwxrwxr-x`: Directory
        .[grepl("^-", .)] %>%
        # File name is at the end, not including a space
        str_extract(pattern = "[^\\s]+$")

    if (!length(remoteFileList)) {
        abort("No files listed on remote server")
    }

    # Apply pattern matching
    remoteFileName <- str_subset(remoteFileList, pattern)
    if (!length(remoteFileName)) {
        abort("Pattern didn't match any files")
    }

    # Rename files, if desired
    if (!is.null(rename)) {
        if (!identical(length(rename), length(remoteFileName))) {
            abort("Rename vector doesn't match the number of remote files")
        }
    }

    # Ensure the local directory exists
    dir.create(localDir, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(quiet)) {
        inform(paste("Downloading", toString(remoteFileName)))
    }
    list <- lapply(seq_along(remoteFileName), function(a) {
        # Rename file, if desired
        if (!is.null(rename)) {
            localFileName <- rename[a]
        } else {
            localFileName <- remoteFileName[a]
        }

        remoteFilePath <- paste0(remoteDir, remoteFileName[a])
        localFilePath <- file.path(localDir, localFileName)
        download.file(
            url = remoteFilePath,
            destfile = localFilePath,
            quiet = quiet)

        # Compress, if desired
        if (isTRUE(compress)) {
            localFilePath <- gzip(localFilePath, overwrite = TRUE)
        }

        localFilePath
    })
    names(list) <- remoteFileName
    invisible(list)
}
