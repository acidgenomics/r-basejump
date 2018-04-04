#' Assert All Variables Are Non-Existing
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @param x Character vector of variable names to check in environment.
#'
#' @export
#'
#' @examples
#' assertAllAreNonExisting(c("XXX", "YYY"))
assertAllAreNonExisting <- function(
    x,
    envir = parent.frame(),
    inherits = FALSE
) {
    exists <- is_existing(x, envir = envir, inherits = inherits)
    if (any(exists)) {
        abort(paste(
            "Already exists in environment:",
            toString(x[exists])
        ))
    }
}
