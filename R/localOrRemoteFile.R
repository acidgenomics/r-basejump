#' Dynamically Handle a Local or Remote File Path
#'
#' This function is vectorized and supports mixed local and remote paths.
#'
#' @family Developer Functions
#'
#' @inheritParams saveData
#' @inheritParams general
#'
#' @return Named `character` containing the original basename as the name and
#'   local file path (i.e. tempfile) as the string. Aborts on a missing file.
#' @export
#'
#' @examples
#' # Single file
#' x <- localOrRemoteFile("http://basejump.seq.cloud/rnaseqCounts.csv.gz")
#' names(x)
#'
#' # Vectorized
#' x <- localOrRemoteFile(c(
#'     "http://basejump.seq.cloud/rnaseqCounts.csv.gz",
#'     "http://basejump.seq.cloud/singleCellCounts.mtx.gz"
#' ))
#' names(x)
localOrRemoteFile <- function(file) {
    assert_is_character(file)

    local <- mapply(
        file = file,
        FUN = function(file) {
            # Remote file mode
            if (grepl("\\://", file)) {
                # Make sure local tempfile always has an extension
                extPattern <- "\\.([A-Za-z0-9]+)$"
                assert_all_are_matching_regex(file, extPattern)
                ext <- str_match(file, extPattern)[, 2L]

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

                destfile <- paste(tempfile(), ext, sep = ".")
                download.file(url = file, destfile = destfile, mode = mode)
                destfile
            } else {
                file
            }
        },
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
    assert_all_are_existing_files(local)

    local <- normalizePath(local, winslash = "/", mustWork = TRUE)
    names(local) <- basename(file)
    local
}
