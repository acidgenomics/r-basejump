#' Dynamically Handle a Local or Remote File Path
#'
#' This function is vectorized and supports mixed local and remote paths.
#'
#' @importFrom fs file_temp path_real
#' @importFrom utils download.file
#'
#' @inheritParams general
#' @inheritParams saveData
#'
#' @param severity Return `stop` (default), `warning`, or `message` if file
#'   doesn't exist.
#'
#' @return Named character vector containing the original basename as the name
#'   and local file path (i.e. tempfile) as the string. Aborts on a missing
#'   file by default. Returns `NULL` when `severity = "warning"` and a missing
#'   file is detected.
#' @export
#'
#' @examples
#' # Single file
#' file <- localOrRemoteFile("http://basejump.seq.cloud/mtcars.csv")
#' names(file)
#'
#' # Vectorized
#' files <- localOrRemoteFile(c(
#'     "http://basejump.seq.cloud/mtcars.csv",
#'     "http://basejump.seq.cloud/mtcars.rda"
#' ))
#' names(files)
localOrRemoteFile <- function(object, severity = "stop") {
    assert_is_character(object)
    .assertFormalSeverity(severity)

    files <- mapply(
        FUN = function(path) {
            # Download remote file, if necessary
            if (grepl("\\://", path)) {
                # Remote file
                file <- file_temp()
                # Fix for Excel files on Windows
                # https://github.com/tidyverse/readxl/issues/374
                # Otherwise, `read_excel()` errors in `readFileByExtension()`
                if (grepl("\\.xlsx$", path)) {
                    # Write binary
                    mode <- "wb"
                } else {
                    # Write (default)
                    mode <- "w"
                }
                download.file(url = path, destfile = file, mode = mode)
            } else {
                file <- path
            }
            file
        },
        path = object,
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE)

    assert_all_are_existing_files(files, severity = severity)
    # Return NULL when severity isn't stop and not all files exist
    if (!all(file_exists(files))) {
        return(NULL)
    }

    files <- path_real(files)
    names(files) <- basename(object)
    files
}
