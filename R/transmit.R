#' Transmit Files from a Remote FTP Server
#'
#' Utility function that supports easy file matching and download from a remote
#' FTP server. Also enables on-the-fly file renaming and compression.
#'
#' @family Data Import and Project Utilities
#'
#' @importFrom fs path_join path_real
#' @importFrom R.utils gzip
#' @importFrom RCurl getURL
#' @importFrom readr read_lines
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
#' readme <- transmit(
#'     remoteDir = "ftp://ftp.ensembl.org/pub/release-90",
#'     pattern = "README",
#'     rename = "ensembl_readme.txt",
#'     compress = TRUE)
#' basename(readme)
#' file_exists(readme)
#'
#' # Clean up
#' file_delete("ensembl_readme.txt.gz")
transmit <- function(
    remoteDir,
    localDir = ".",
    pattern,
    rename = NULL,
    compress = FALSE) {
    assert_is_a_string(remoteDir)
    # Check for public FTP protocol
    assert_all_are_matching_regex(remoteDir, "^ftp\\://")
    # `RCurl::getURL()` requires a trailing slash
    if (!grepl("/$", remoteDir)) {
        remoteDir <- paste0(remoteDir, "/")
    }
    localDir <- initializeDirectory(localDir)
    assert_is_a_string(pattern)
    assertIsCharacterOrNULL(rename)
    assert_is_a_bool(compress)

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
    match <- str_subset(remoteFileList, pattern)
    assert_is_non_empty(match)

    # Concatenate using paste but strip the trailing slash (see above)
    remotePath <- paste(gsub("/$", "", remoteDir), match, sep = "/")

    # Rename files, if desired
    if (is.character(rename)) {
        assert_are_same_length(match, rename)
        name <- rename
    } else {
        name <- match
    }
    localPath <- path(localDir, name)

    inform(paste("Downloading", toString(match)))

    files <- mapply(
        FUN = function(url, destfile, compress) {
            download.file(url = url, destfile = destfile)
            # Compress, if desired
            if (isTRUE(compress)) {
                destfile <- gzip(destfile, overwrite = TRUE)
            }
            destfile
        },
        url = remotePath,
        destfile = localPath,
        MoreArgs = list(compress = compress),
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE)

    files <- path_real(files)
    names(files) <- match

    invisible(files)
}
