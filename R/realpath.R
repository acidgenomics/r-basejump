#' @inherit base::normalizePath
#' @export
#' @seealso [base::normalizePath()].
#' @examples
#' realpath(".")
#' normalizePath(".")
realpath <- function(path) {
    assert_is_character(path)
    normalizePath(path = path, mustWork = TRUE)
}
