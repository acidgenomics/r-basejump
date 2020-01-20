#' Transmit files from a remote FTP server
#'
#' Utility function that supports easy file matching and download from a remote
#' FTP server. Also enables on-the-fly file renaming and compression.
#'
#' @export
#' @note Updated 2020-01-18.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams saveData
#' @param remoteDir `character(1)`.
#'   Remote FTP directory path.
#' @param localDir `character(1)`.
#'   Directory where to save files locally.
#' @param rename `character(1)` or `NULL`.
#'   Rename the local files (including suffix), if desired.
#' @param compress `logical(1)`.
#'   gzip compress the files after download.
#'
#' @return Invisible `character`.
#' Local file paths.
#'
#' @examples
#' if (
#'     goalie::hasInternet() &&
#'     !isTRUE(nzchar(Sys.getenv("CI")))
#' ) {
#'     remoteDir <- paste(
#'         "ftp://ftp.ncbi.nlm.nih.gov",
#'         "genomes",
#'         "Homo_sapiens",
#'         sep = "/"
#'     )
#'     readme <- transmit(
#'         remoteDir = remoteDir,
#'         pattern = "README",
#'         rename = "ncbi-readme.txt",
#'         compress = FALSE
#'     )
#'     basename(readme)
#'     file.exists(readme)
#'     unlink(readme)
#' }
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
        ## Check for public FTP protocol. Note that we're wrapping with `all()`
        ## here because the check is parameterized, and includes name, which
        ## causes the check to fail on R 3.4.
        all(isMatchingRegex(remoteDir, "^ftp\\://"))
    )
    ## `RCurl::getURL()` requires a trailing slash.
    if (!grepl("/$", remoteDir)) {
        remoteDir <- paste0(remoteDir, "/")
    }
    localDir <- initDir(localDir)
    assert(
        isString(pattern),
        isAny(rename, classes = c("character", "NULL")),
        isFlag(compress)
    )
    ## Get the name of the server.
    server <- str_match(remoteDir, "^.*//([^/]+)/.*$")[1L, 2L]
    assert(isString(server))
    ## Error and inform the user if the FTP connection fails.
    if (!isTRUE(url.exists(remoteDir))) {
        stop(sprintf("Connection to '%s' failed.", server))  # nocov
    } else {
        cli_text(sprintf("Transmitting files from {.url %s}.", server))
    }
    ## Get a list of the files in the remote directory.
    remoteTxt <- getURL(remoteDir)
    if (!all(
        is.character(remoteTxt),
        length(remoteTxt) > 0L
    )) {
        stop("Failed to list directory contents.")  # nocov
    }
    ## Match the `-` at begining for file.
    ## `-rwxrwxr-x`: File
    ## `drwxrwxr-x`: Directory
    remoteFiles <- unlist(
        x = strsplit(
            x = remoteTxt,
            split = "\n",
            fixed = TRUE
        ),
        recursive = FALSE,
        use.names = FALSE
    )
    remoteFiles <- remoteFiles[grepl("^-", remoteFiles)]
    ## File name is at the end, not including a space.
    remoteFiles <- str_extract(remoteFiles, "[^\\s]+$")
    assert(hasLength(remoteFiles))
    ## Apply pattern matching.
    match <- str_subset(remoteFiles, pattern)
    assert(hasLength(match))
    cli_alert_info(sprintf(
        "Files matching pattern: {.file %s}",
        toString(match, width = 200L)
    ))
    ## Concatenate using paste but strip the trailing slash (see above).
    remotePaths <- paste(gsub("/$", "", remoteDir), match, sep = "/")
    ## Rename files, if desired.
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
    ## Check for existing files and skip, if necessary.
    if (any(file.exists(files))) {
        exists <- which(file.exists(files))
        skip <- files[exists]
        cli_alert_warning(sprintf(
            "Skipped: {.file %s}.",
            toString(basename(skip), width = 200L)
        ))
        localPaths <- localPaths[!exists]
    }
    ## Early return if all files exist.
    if (length(localPaths) == 0L) {
        cli_alert_success("All files are already downloaded.")
        files <- realpath(files)
        names(files) <- match
        return(invisible(files))
    }
    ## Download and return file paths.
    cli_alert(sprintf(
        "Downloading {.file %s}.",
        toString(basename(files), width = 200L)
    ))
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
