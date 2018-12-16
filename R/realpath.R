#' Express file paths in canonical form
#' @inherit base::normalizePath
#' @export
#' @seealso
#' - [base::file.path()].
#' - [base::normalizePath()].
#' @examples
#' realpath(".")
#' normalizePath(".")
realpath <- function(path) {
    assert(
        isCharacter(path),
        all(hasAccess(path))
    )
    # Ensure we're matching the platform conventions.
    # For example, AppVeyor CI tests on Windows but uses "/" instead of "\\".
    normalizePath(
        path = path,
        winslash = .Platform$file.sep,  # nolint
        mustWork = TRUE
    )
}
