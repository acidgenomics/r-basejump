#' Transmit (download) files from a remote server
#'
#' Utility function that supports file matching on a remote server. Also enables
#' users to rename and compress on the fly.
#'
#' @param remote_dir Remote directory URL.
#' @param pattern Pattern to match against remote file names.
#' @param rename Rename the local file (including suffix), if desired.
#' @param compress Compress the file with [gzip()] after download.
#'   (`TRUE`/`FALSE`)
#' @param local_dir Directory where to save file locally.
#'
#' @return List of local files.
#' @export
transmit <- function(
    remote_dir,
    pattern = NULL,
    rename = NULL,
    compress = FALSE,
    local_dir = "data-raw") {
    remote_file_name <- getURL(remote_dir, dirlistonly = TRUE) %>% read_lines
    if (!length(remote_file_name)) {
        stop("No files listed on remote server")
    }

    # Apply pattern matching
    if (!is.null(pattern)) {
        remote_file_name <- str_subset(remote_file_name, pattern)
        if (!length(remote_file_name)) {
            stop("Pattern didn't match any files")
        }
    }

    # Rename files, if desired
    if (!is.null(rename)) {
        if (length(rename) != length(remote_file_name)) {
            stop("Rename vector doesn't match the number of remote files")
        }
    }

    # Ensure the local directory exists
    dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
    message(toString(remote_file_name))
    list <- lapply(seq_along(remote_file_name), function(a) {
        # Rename file, if desired
        if (!is.null(rename)) {
            local_file_name <- rename[a]
        } else {
            local_file_name <- remote_file_name[a]
        }
        remote_file_path <- paste0(remote_dir, remote_file_name[a])
        local_file_path <- file.path(local_dir, local_file_name)
        download.file(remote_file_path, local_file_path)
        # Compress, if desired
        if (isTRUE(compress)) {
            local_file_name <- gzip(local_file_path, overwrite = TRUE)
        } else {
            local_file_name <- remote_file_name
        }
        local_file_name
    }
    ) %>% setNames(remote_file_name)
    list
}
