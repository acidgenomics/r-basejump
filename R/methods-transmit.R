#' Transmit Files from a Remote FTP Server
#'
#' Utility function that supports easy file matching and download from a remote
#' FTP server. Also enables on-the-fly file renaming and compression.
#'
#' @rdname transmit
#' @name transmit
#'
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
#' transmit("ftp://ftp.ensembl.org/pub/release-89",
#'          pattern = "README",
#'          rename = "ensembl_readme.txt",
#'          compress = TRUE)
NULL



# Methods ====
#' @rdname transmit
#' @export
setMethod("transmit", "character", function(
    object,
    pattern = NULL,
    rename = NULL,
    compress = FALSE,
    localDir = "data-raw") {
    remoteDir <- object
    if (!str_detect(remoteDir, "ftp\\://")) {
        stop("FTP protocol not detected")
    }
    # Fix trailing slash, if necessary
    if (!str_detect(remoteDir, "/$")) {
        remoteDir <- paste0(remoteDir, "/")
    }

    remoteFileList <- remoteDir %>%
        getURL(dirlistonly = TRUE) %>%
        read_lines
    if (!length(remoteFileList)) {
        stop("No files listed on remote server")
    }

    # Apply pattern matching
    if (!is.null(pattern)) {
        remoteFileName <- str_subset(remoteFileList, pattern)
        if (!length(remoteFileName)) {
            stop("Pattern didn't match any files")
        }
    }

    # Rename files, if desired
    if (!is.null(rename)) {
        if (length(rename) != length(remoteFileName)) {
            stop("Rename vector doesn't match the number of remote files")
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
            localFileName <- gzip(localFilePath, overwrite = TRUE)
        } else {
            localFileName <- remoteFileName
        }

        localFileName
    }) %>%
        setNames(remoteFileName)
})
