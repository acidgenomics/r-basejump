# TODO Need to improve error messages here rather than simply using asserts.

#' Transmit files from a remote FTP server
#'
#' Utility function that supports easy file matching and download from a remote
#' FTP server. Also enables on-the-fly file renaming and compression.
#'
#' @inheritParams params
#' @inheritParams saveData
#' @export
#'
#' @param remoteDir `character(1)`.
#'   Remote FTP directory path.
#' @param localDir `character(1)`.
#'   Directory where to save files locally.
#' @param pattern `character(1)`.
#'   Pattern to match against remote file names.
#' @param rename `character(1)` or `NULL`.
#'   Rename the local files (including suffix), if desired.
#' @param compress `logical(1)`.
#'   gzip compress the files after download.
#'
#' @return Invisible `character`.
#' Local file paths.
#'
#' @examples
#' remoteDir <- paste(
#'     "ftp://ftp.pantherdb.org",
#'     "sequence_classifications",
#'     "current_release",
#'     sep = "/"
#' )
#' readme <- transmit(
#'     remoteDir = remoteDir,
#'     pattern = "README",
#'     rename = "panther_readme.txt",
#'     compress = TRUE
#' )
#' basename(readme)
#' file.exists(readme)
#'
#' ## Clean up.
#' unlink(readme)
transmit <- function(
    remoteDir,
    localDir = ".",
    pattern,
    rename = NULL,
    compress = FALSE
) {
    assert(
        hasInternet(),
        isString(remoteDir),
        # Check for public FTP protocol.
        isMatchingRegex(remoteDir, "^ftp\\://")
    )
    # `RCurl::getURL` requires a trailing slash.
    if (!grepl("/$", remoteDir)) {
        remoteDir <- paste0(remoteDir, "/")
    }
    localDir <- initDir(localDir)
    assert(
        isString(pattern),
        isAny(rename, classes = c("character", "NULL")),
        isFlag(compress)
    )

    # Get the name of the server.
    server <- str_match(string = remoteDir, pattern = "^.*//([^/]+)/.*$") %>%
        .[1L, 2L]
    assert(isString(server))

    # Error and inform the user if the FTP connection fails.
    if (!isTRUE(url.exists(remoteDir))) {
        stop(paste("Connection to", server, "failed."))
    } else {
        message(paste0("Transmitting files from ", server, "."))
    }

    remoteTxt <- getURL(remoteDir)
    if (!isTRUE(
        is.character(remoteTxt) &&
        length(remoteTxt) > 0L
    )) {
        stop("Failed to list directory contents.")
    }

    # Match the `-` at begining for file.
    # `-rwxrwxr-x`: File
    # `drwxrwxr-x`: Directory
    remoteFiles <- remoteTxt %>%
        read_lines() %>%
        .[grepl("^-", .)] %>%
        # File name is at the end, not including a space.
        str_extract(pattern = "[^\\s]+$")
    assert(hasLength(remoteFiles))

    # Apply pattern matching.
    match <- str_subset(remoteFiles, pattern)
    assert(hasLength(match))

    message(paste(
        "Files matching pattern:",
        printString(match),
        sep = "\n"
    ))

    # Concatenate using paste but strip the trailing slash (see above).
    remotePaths <- paste(gsub("/$", "", remoteDir), match, sep = "/")

    # Rename files, if desired.
    if (is.character(rename)) {
        assert(areSameLength(x = match, y = rename))
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

    # Check for existing files and skip, if necessary.
    if (any(file.exists(files))) {
        exists <- which(file.exists(files))
        skip <- files[exists]
        message(paste0("Skipped ", toString(basename(skip)), "."))
        localPaths <- localPaths[!exists]
    }

    # Early return if all files exist.
    if (length(localPaths) == 0L) {
        message("All files are already downloaded.")
        files <- realpath(files)
        names(files) <- match
        return(invisible(files))
    }

    message(paste0("Downloading ", toString(basename(files)), "."))
    files <- mapply(
        FUN = function(url, destfile, compress = FALSE) {
            download.file(url = url, destfile = destfile)
            if (isTRUE(compress)) {
                destfile <- gzip(destfile, overwrite = TRUE)
            }
            realpath(destfile)
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
