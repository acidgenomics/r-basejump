#' Initialize Directory
#'
#' Checks and creates a directory recursively automatically. Useful for setting
#' up functions that require writes to a user-specified directory.
#'
#' @family Write Functions
#'
#' @param dir Directory path.
#'
#' @return Path.
#' @export
#'
#' @examples
#' initializeDirectory("testdir")
#' dir.exists("testdir")
#'
#' # Clean up
#' unlink("testdir", recursive = TRUE)
initializeDirectory <- function(dir) {
    assert_is_a_string(dir)
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    normalizePath(dir, winslash = "/", mustWork = TRUE)
}
