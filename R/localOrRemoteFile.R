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
#' @return Named list containing the original basename as the name and local
#'   file path (i.e. tempfile) as the string. Aborts on a missing file.
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
localOrRemoteFile <- function(
    object,
    severity = "stop",
    quiet = FALSE) {
    assert_is_character(object)
    assert_is_a_string(severity)
    assert_is_subset(severity, c("message", "stop", "warning"))
    assert_is_a_bool(quiet)

    files <- mapply(
        FUN = function(path, quiet) {
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

                download.file(
                    url = path,
                    destfile = file,
                    quiet = quiet,
                    mode = mode)
            } else {
                file <- path
            }
            assert_all_are_existing_files(file)
            path_real(file)
        },
        path = object,
        MoreArgs = list(quiet = quiet),
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE)
    assert_is_character(files)
    names(files) <- basename(object)

    assert_all_are_existing_files(as.character(files), severity = severity)
    files
}
