#' Assign Multiple Objects as a New Environment
#'
#' @family Object Assignment Utilities
#'
#' @importFrom rlang is_string
#'
#' @inheritParams dots
#' @inheritParams saveData
#'
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
    parentEnvir = parent.frame(),
    quiet = FALSE) {
    if (!is_string(envirName)) {
        abort("`envirName` must be a string")
    }
    if (!is.environment(parentEnvir)) {
        abort("`parentEnvir` must be an environment")
    }
    dots <- dots(...)
    dotsNames <- dots(..., character = TRUE)
    envir <- new.env(parent = parentEnvir)
    lapply(seq_along(dots), function(a) {
        assign(dotsNames[[a]], eval(dots[[a]]), envir = envir)
    }) %>%
        invisible()
    if (!isTRUE(quiet)) {
        inform(paste("Assigning", toString(dotsNames), "as", envirName))
    }
    assign(envirName, value = envir, envir = parentEnvir)
    # Silently return a list of the objects in the new environment
    invisible(objects(envir))
}
