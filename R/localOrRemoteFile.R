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
#'   local file path (i.e. tempfile) as the string. Stops on a missing file.
#' @export
#'
#' @examples
#' # Single file
#' x <- localOrRemoteFile("http://basejump.seq.cloud/rnaseqCounts.csv.gz")
#' basename(x)
#'
#' # Vectorized
#' x <- localOrRemoteFile(c(
#'     "http://basejump.seq.cloud/rnaseqCounts.csv.gz",
#'     "http://basejump.seq.cloud/singleCellCounts.mtx.gz"
#' ))
#' basename(x)
localOrRemoteFile <- function(file) {
    assert_is_character(file)
    local <- mapply(
        file = file,
        FUN = function(file) {
            # Remote file mode
            if (isURL(file)) {
                assert_all_are_matching_regex(file, extPattern)
                ext <- str_match(basename(file), extPattern) %>%
                    .[1L, 2L:3L] %>%
                    na.omit() %>%
                    paste(collapse = "")
                assert_is_non_empty(ext)

                # Fix for binary files (typically on Windows)
                # https://github.com/tidyverse/readxl/issues/374
                # Otherwise, `read_excel()` errors in `readFileByExtension()`
                if (ext %in% c("rda", "rds", "xls", "xlsx")) {
                    # Write binary
                    mode <- "wb"
                } else {
                    # Write (default)
                    mode <- "w"
                }

                destfile <- file.path(tempdir(), basename(file))
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
    assert_all_are_matching_regex(local, extPattern)
    normalizePath(local, winslash = "/", mustWork = TRUE)
}
