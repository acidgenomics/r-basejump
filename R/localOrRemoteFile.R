#' Dynamically Handle a Local or Remote File Path
#'
#' @importFrom utils download.file
#'
#' @inheritParams general
#' @inheritParams saveData
#'
#' @param severity Return `stop` (default), `warning`, or `message` if file
#'   doesn't exist.
#'
#' @return Named character vector containing the original file name as the
#'   name and local file path as the string. Aborts on a missing file.
#' @export
#'
#' @examples
#' localOrRemoteFile("http://basejump.seq.cloud/mtcars.rda")
localOrRemoteFile <- function(
    object,
    severity = "stop",
    quiet = FALSE) {
    assert_is_a_string(object)
    assert_is_a_string(severity)
    assert_is_subset(severity, c("message", "stop", "warning"))
    assert_is_a_bool(quiet)
    basename <- basename(object)

    # Download remote file, if necessary
    if (grepl("\\://", object)) {
        # Remote file
        file <- tempfile()
        download.file(url = object, destfile = file, quiet = quiet)
    } else {
        file <- object
    }

    assert_all_are_existing_files(file, severity = severity)
    if (!file.exists(file)) {
        return(invisible())
    }

    file <- path_real(file)
    names(file) <- basename
    file
}
