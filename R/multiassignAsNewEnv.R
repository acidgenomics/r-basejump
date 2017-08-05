#' Assign Multiple Objects as a New Environment
#'
#' @rdname assignIntoNewEnv
#' @family Object Assignment Utilities
#'
#' @inheritParams dots
#' @param newEnv New environment name (string).
#' @param parentEnv Parent environment of new environment.
#'
#' @return Character vector of assigned objects in the new environment.
#' @export
#'
#' @seealso [Biobase::multiassign()].
#'
#' @examples
#' multiassignAsNewEnv(mtcars, starwars, newEnv = "test")
multiassignAsNewEnv <- function(..., newEnv, parentEnv = parent.frame()) {
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
    message(paste("Assigning", toString(dotsNames), "as", newEnv))
    assign(newEnv, env, envir = parentEnv)
    objects(env)
}
