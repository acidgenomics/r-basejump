#' Dynamically Handle a Local or Remote File Path
#'
#' This function is vectorized and supports mixed local and remote paths.
#'
#' @family Read Functions
#'
#' @inheritParams saveData
#' @inheritParams general
#'
#' @return Named character vector containing the original basename as the name
#'   and local file path (i.e. tempfile) as the string. Aborts on a missing
#'   file.
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
localOrRemoteFile <- function(object) {
    assert_is_character(object)

    # Vectorized support
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

    assert_all_are_existing_files(files)
    files <- normalizePath(files, winslash = "/", mustWork = TRUE)
    names(files) <- basename(object)
    files
}
