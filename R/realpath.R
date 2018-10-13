#' @inherit base::normalizePath
#' @export
#' @seealso [base::normalizePath()].
#' @examples
#' realpath(".")
#' normalizePath(".")
realpath <- function(path) {
    assert_is_character(path)
    assert_all_are_existing_files(path)
    normalizePath(path = path, mustWork = TRUE)
}
