# TODO Use assert engine and add severity

#' Assert All Variables Are Non-Existing
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @param x Character vector of variable names to check in environment.
#'
#' @export
#'
#' @examples
#' a <- 1L
#' b <- 2L
#' tryCatch(
#'     assertAllAreNonExisting(c("a", "b", "c")),
#'     error = function(e) e
#' )
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
