#' Dynamically Handle a Local or Remote File Path
#'
#' This function is vectorized and supports mixed local and remote paths.
#'
#' @family Read Functions
#'
#' @inheritParams general
#' @inheritParams saveData
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
localOrRemoteFile <- function(
    object,
    severity = c("stop", "warning", "message", "none")
) {
    assert_is_character(object)
    severity <- match.arg(severity)

    files <- mapply(
        FUN = function(path) {
            if (file.exists(path)) {
                # Local file mode
                path
            } else if (grepl("\\://", path)) {
                # Remote file mode
                tempfile <- tempfile()

                # Make sure local tempfile always has an extension
                extPattern <- "\\.([A-Za-z0-9]+)$"
                assert_all_are_matching_regex(path, extPattern)
                ext <- str_match(path, extPattern)[, 2L]

                # Fix for Excel files on Windows
                # https://github.com/tidyverse/readxl/issues/374
                # Otherwise, `read_excel()` errors in `readFileByExtension()`
                if (ext == "xlsx") {
                    # Write binary
                    mode <- "wb"
                } else {
                    # Write (default)
                    mode <- "w"
                }

                destfile <- paste(tempfile, ext, sep = ".")
                download.file(url = path, destfile = destfile, mode = mode)
                destfile
            } else {
                abort(paste("Invalid path:", path))
            }
        },
        path = object,
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )

    assert_all_are_existing_files(files, severity = severity)
    # Return NULL when severity isn't stop and not all files exist
    if (!all(file.exists(files))) {
        return(NULL)
    }

    files <- normalizePath(files, winslash = "/", mustWork = TRUE)
    names(files) <- basename(object)
    files
}
