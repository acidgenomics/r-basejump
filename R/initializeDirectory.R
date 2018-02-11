#' Initialize Directory
#'
#' Checks and creates a directory recursively automatically. Useful for setting
#' up functions that require writes to a user-specified directory.
#'
#' @param dir Directory path.
#'
#' @return Directory path string.
#' @export
#'
#' @examples
#' initializeDirectory("testdir")
#' dir.exists("testdir")
#' unlink("testdir")
initializeDirectory <- function(dir) {
    assert_is_a_string(dir)
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
    }
    normalizePath(dir)
}
