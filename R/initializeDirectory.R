#' Initialize Directory
#'
#' Checks and creates a directory recursively automatically. Useful for setting
#' up functions that require writes to a user-specified directory.
#'
#' @importFrom fs dir_create dir_exists path_real
#'
#' @param dir Directory path.
#'
#' @return Directory path string.
#' @export
#'
#' @examples
#' initializeDirectory("testdir")
#' dir_exists("testdir")
#' dir_delete("testdir", recursive = TRUE)
initializeDirectory <- function(dir) {
    assert_is_a_string(dir)
    if (!dir_exists(dir)) {
        dir_create(dir, recursive = TRUE)
    }
    path_real(dir)
}
