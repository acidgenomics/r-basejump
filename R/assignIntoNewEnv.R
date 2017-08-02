#' Assign Multiple Objects to a New Environment
#'
#' @rdname assignIntoNewEnv
#' @family Object Assignment Utilities
#'
#' @inheritParams dots
#' @param envName New environment name.
#' @param parentEnv Parent environment of new environment.
#'
#' @return No value.
#' @export
#'
#' @seealso [Biobase::multiassign()].
#'
#' @examples
#' assignIntoNewEnv(mtcars, starwars, envName = "test")
assignIntoNewEnv <- function(..., envName, parentEnv = parent.frame()) {
    if (!is_string(envName)) {
        stop("Environment name must be a string")
    }
    print(parentEnv)

    dots <- dots(...)
    dotsNames <- dots(..., character = TRUE)

    newEnv <- new.env()
    lapply(seq_along(dots), function(a) {
        assign(dotsNames[[a]], eval(dots[[a]]), envir = newEnv)
    }) %>%
        invisible
    message(paste("Assigning",
                  toString(dotsNames),
                  "to",
                  envName))

    assign(envName, newEnv, envir = parentEnv)
}
