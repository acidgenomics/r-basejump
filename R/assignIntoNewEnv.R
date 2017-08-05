#' Assign Multiple Objects to a New Environment
#'
#' @rdname assignIntoNewEnv
#' @family Object Assignment Utilities
#'
#' @inheritParams dots
#' @param newEnv New environment name (string).
#' @param parentEnv Parent environment of new environment.
#'
#' @return No value.
#' @export
#'
#' @seealso [Biobase::multiassign()].
#'
#' @examples
#' assignIntoNewEnv(mtcars, starwars, newEnv = "test")
assignIntoNewEnv <- function(..., newEnv, parentEnv = parent.frame()) {
    if (!is_string(newEnv)) {
        stop("Environment name must be a string", call. = FALSE)
    }
    dots <- dots(...)
    dotsNames <- dots(..., character = TRUE)
    env <- new.env(parent = parentEnv)
    lapply(seq_along(dots), function(a) {
        assign(dotsNames[[a]], eval(dots[[a]]), envir = env)
    }) %>%
        invisible
    message(paste("Assigning",
                  toString(dotsNames),
                  "as",
                  newEnv,
                  "into",
                  environmentName(parentEnv)))
    assign(newEnv, env, envir = parentEnv)
}
