#' Transmit Files from a Remote FTP Server
#'
#' Utility function that supports easy file matching and download from a remote
#' FTP server. Also enables on-the-fly file renaming and compression.
#'
#' @family Write Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams saveData
#' @inheritParams general
#' @param remoteDir `string`. Remote FTP directory path.
#' @param localDir `string`. Directory where to save files locally.
#' @param pattern `string`. Pattern to match against remote file names.
#' @param rename `string` or `NULL`. Rename the local files (including suffix),
#'   if desired.
#' @param compress `boolean`. gzip compress the files after download.
#'
#' @return Invisible `character` containing local file paths.
#' @export
#'
#' @examples
#' readme <- transmit(
#'     remoteDir = "ftp://ftp.ensembl.org/pub/release-90",
#'     pattern = "README",
#'     rename = "ensembl_readme.txt",
#'     compress = TRUE
#' )
#' basename(readme)
#' file.exists(readme)
#'
#' # Clean up
#' unlink("ensembl_readme.txt.gz")
transmit <- function(
    remoteDir,
    localDir = ".",
    pattern,
    rename = NULL,
    compress = FALSE
) {
    stopifnot(has_internet())
    assert_is_a_string(remoteDir)
    # Check for public FTP protocol
    assert_all_are_matching_regex(remoteDir, "^ftp\\://")
    # `RCurl::getURL()` requires a trailing slash
    if (!grepl("/$", remoteDir)) {
        remoteDir <- paste0(remoteDir, "/")
    }
    localDir <- initializeDirectory(localDir)
    assert_is_a_string(pattern)
    assert_is_any_of(rename, c("character", "NULL"))
    assert_is_a_bool(compress)

    remoteList <- remoteDir %>%
        getURL() %>%
        read_lines()
    assert_is_non_empty(remoteList)

    # Match the `-` at begining for file
    # `-rwxrwxr-x`: File
    # `drwxrwxr-x`: Directory
    remoteFiles <- remoteList %>%
        .[grepl("^-", .)] %>%
        # File name is at the end, not including a space
        str_extract(pattern = "[^\\s]+$")
    assert_is_non_empty(remoteFiles)

    # Apply pattern matching
    match <- str_subset(remoteFiles, pattern)
    assert_is_non_empty(match)

    # Concatenate using paste but strip the trailing slash (see above)
    remotePaths <- paste(gsub("/$", "", remoteDir), match, sep = "/")

    # Rename files, if desired
    if (is.character(rename)) {
        assert_are_same_length(match, rename)
        name <- rename
    } else {
        name <- match
    }

    localPaths <- file.path(localDir, name)

    if (isTRUE(compress)) {
        files <- paste(localPaths, "gz", sep = ".")
    } else {
        files <- localPaths
    }

    # Check for existing files and skip, if necessary
    if (any(file.exists(files))) {
        exists <- which(file.exists(files))
        skip <- files[exists]
        message(paste("Skipping", toString(basename(skip))))
        localPaths <- localPaths[!exists]
    }

    # Early return if all files exist
    if (!length(localPaths)) {
        message("All files have already downloaded")
        files <- normalizePath(files, winslash = "/", mustWork = TRUE)
        names(files) <- match
        return(invisible(files))
    }

    message(paste("Downloading", toString(basename(files))))
    files <- mapply(
        FUN = function(url, destfile, compress = FALSE) {
            download.file(url = url, destfile = destfile)
            if (isTRUE(compress)) {
                destfile <- gzip(destfile, overwrite = TRUE)
            }
            normalizePath(destfile, winslash = "/", mustWork = TRUE)
        },
        url = remotePaths,
        destfile = localPaths,
        MoreArgs = list(compress = compress),
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
    names(files) <- match
    invisible(files)
}
