#' Dynamically Handle a Local or Remote File Path
#'
#' @importFrom utils download.file
#'
#' @inheritParams AllGenerics
#' @inheritParams saveData
#'
#' @return Named character vector containing the original file name as the
#'   name and local file path as the string. Aborts on a missing file.
#' @export
#'
#' @examples
#' localOrRemoteFile("http://basejump.seq.cloud/mtcars.rda")
localOrRemoteFile <- function(object, quiet = FALSE) {
    assert_is_a_string(object)
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

    assert_all_are_existing_files(file)
    file <- normalizePath(file)
    names(file) <- basename
    file
}
