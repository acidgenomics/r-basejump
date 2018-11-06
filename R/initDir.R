#' Initialize Directory
#'
#' Checks and creates a directory recursively automatically.
#'
#' Useful for setting up functions that require writes to a user-specified
#' directory.
#'
#' @export
#'
#' @param dir `string`. Directory path.
#'
#' @return `string`. Directory path.
#'
#' @examples
#' initDir("testdir")
#' dir.exists("testdir")
#'
#' ## Clean up.
#' unlink("testdir", recursive = TRUE)
initDir <- function(dir) {
    assert_is_a_string(dir)
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    realpath(dir)
}
