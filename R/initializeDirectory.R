#' Initialize Directory
#'
#' @param dir Directory path.
#'
#' @examples
#' initializeDirectory(getwd())
initializeDirectory <- function(dir) {
    assert_is_a_string(dir)
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
    }
    normalizePath(dir)
}
