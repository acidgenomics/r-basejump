#' @inherit base::normalizePath
#' @export
#' @seealso [base::normalizePath()].
#' @examples
#' realpath(".")
#' normalizePath(".")
realpath <- function(path) {
    normalizePath(path = path, mustWork = TRUE)
}
