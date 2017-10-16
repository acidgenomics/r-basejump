#' Assign Multiple Objects as a New Environment
#'
#' @family Object Assignment Utilities
#'
#' @importFrom rlang is_string
#'
#' @inheritParams dots
#' @param envirName New environment name (string).
#' @param parentEnvir Parent environment of new environment.
#'
#' @return Character vector of assigned objects in the new environment.
#' @export
#'
#' @seealso [Biobase::multiassign()].
#'
#' @examples
#' multiassignAsNewEnvir(mtcars, starwars, envirName = "test")
multiassignAsNewEnvir <- function(
    ...,
    envirName,
    parentEnvir = parent.frame()) {
    if (!is_string(envirName)) {
        stop("'envirName' must be a string", call. = FALSE)
    }
    if (!is.environment(parentEnvir)) {
        stop("'parentEnvir' must be an environment", call. = FALSE)
    }
    dots <- dots(...)
    dotsNames <- dots(..., character = TRUE)
    envir <- new.env(parent = parentEnvir)
    lapply(seq_along(dots), function(a) {
        assign(dotsNames[[a]], eval(dots[[a]]), envir = envir)
    }) %>%
        invisible()
    message(paste("Assigning", toString(dotsNames), "as", envirName))
    assign(envirName, value = envir, envir = parentEnvir)
    # Silently return a list of the objects in the new environment
    invisible(objects(envir))
}
