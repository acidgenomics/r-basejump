#' Initialize Directory
#'
#' Checks and creates a directory recursively automatically.
#'
#' Useful for setting up functions that require writes to a user-specified
#' directory.
#'
#' @export
#'
#' @param dir `character(1)`.
#'   Directory path.
#'
#' @return `character(1)`.
#'   Directory path.
#'
#' @examples
#' initDir("testdir")
#' dir.exists("testdir")
#'
#' ## Clean up.
#' unlink("testdir", recursive = TRUE)
initDir <- function(dir) {
    assert(isString(dir))
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    realpath(dir)
}
