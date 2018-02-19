# TODO Use assert engine and add severity

#' All Variables Do Not Exist Assert Check
#'
#' @family Assert Checks
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
#'     error = function(e) e)
assertAllAreNonExisting <- function(
    x,
    envir = parent.frame(),
    inherits = FALSE) {
    exists <- is_existing(x, envir = envir, inherits = inherits)
    if (any(exists)) {
        abort(paste(
            "Already exists in environment:",
            toString(x[exists])
        ))
    }
}



# Aliases ======================================================================
#' @rdname assertAllAreNonExisting
#' @export
assertAllAreNonExisting -> assert_all_are_non_existing
