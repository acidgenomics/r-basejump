#' Dynamically Handle a Local or Remote File Path
#'
#' This function is vectorized and supports mixed local and remote paths.
#'
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
#' files <- localOrRemoteFile(c(
#'     "http://basejump.seq.cloud/mtcars.csv",
#'     "http://basejump.seq.cloud/mtcars.rda"
#' ))
#' glimpse(files)
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
                download.file(url = path, destfile = file, quiet = quiet)
            } else {
                file <- path
            }
            path_real(file)
        },
        path = object,
        MoreArgs = list(quiet = quiet),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE)
    names(files) <- basename(object)

    assert_all_are_existing_files(as.character(files), severity = severity)
    files
}
