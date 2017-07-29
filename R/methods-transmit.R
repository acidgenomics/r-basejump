#' Transmit (Download) Files from a Remote Server
#'
#' Utility function that supports file matching on a remote server. Also enables
#' users to rename and compress on the fly.
#'
#' @rdname transmit
#'
#' @param object Remote directory URL.
#' @param pattern Pattern to match against remote file names.
#' @param rename Rename the local file (including suffix), if desired.
#' @param compress Compress the file with [gzip()] after download.
#'   (`TRUE`/`FALSE`)
#' @param localDir Directory where to save file locally.
#'
#' @return List of local files.



#' @rdname transmit
#' @export
setMethod("transmit", "character", function(
    object,
    pattern = NULL,
    rename = NULL,
    compress = FALSE,
    localDir = "data-raw") {
    remoteDir <- object
    remoteFileName <- getURL(remoteDir, dirlistonly = TRUE) %>%
        read_lines
    if (!length(remoteFileName)) {
        stop("No files listed on remote server")
    }

    # Apply pattern matching
    if (!is.null(pattern)) {
        remoteFileName <- str_subset(remoteFileName, pattern)
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
    message(toString(remoteFileName))
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
        set_names(remoteFileName)
})
