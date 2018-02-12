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
#' @inheritParams general
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
    assert_is_a_string(remoteDir)
    # Check for public FTP protocol
    assert_all_are_matching_regex(remoteDir, "^ftp\\://")
    # Append trailing slash, if necessary
    if (!grepl("/$", remoteDir)) {
        remoteDir <- paste0(remoteDir, "/")
    }
    localDir <- initializeDirectory(localDir)
    assert_is_a_string(pattern)
    assert_is_character_or_null(rename)
    assert_is_a_bool(compress)
    assert_is_a_bool(quiet)

    remoteList <- remoteDir %>%
        getURL() %>%
        read_lines()
    assert_is_non_empty(remoteList)

    remoteFileList <- remoteList %>%
        # Match the `-` at begining for file
        # `-rwxrwxr-x`: File
        # `drwxrwxr-x`: Directory
        .[grepl("^-", .)] %>%
        # File name is at the end, not including a space
        str_extract(pattern = "[^\\s]+$")
    assert_is_non_empty(remoteFileList)

    # Apply pattern matching
    remoteFileName <- str_subset(remoteFileList, pattern)
    assert_is_non_empty(remoteFileName)

    # Rename files, if desired
    if (is.character(rename)) {
        assert_all_are_same_length(remoteFileName, rename)
    }

    if (!isTRUE(quiet)) {
        inform(paste("Downloading", toString(remoteFileName)))
    }

    list <- lapply(seq_along(remoteFileName), function(a) {
        # Rename file, if desired
        if (is.character(rename)) {
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
