#' Assign Multiple Objects as a New Environment
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams dots
#' @inheritParams saveData
#' @param envirName New environment name (`string`).
#' @param parentEnvir Parent `environment` where to assign the new environment.
#'
#' @return `character` of object names in the new `environment`.
#' @export
#'
#' @examples
#' multiassignAsNewEnvir(mtcars, starwars, envirName = "test")
multiassignAsNewEnvir <- function(
    ...,
    envirName,
    parentEnvir = parent.frame()
) {
    dots <- dots(...)
    assert_is_list(dots)
    dotsNames <- dots(..., character = TRUE)
    assert_is_character(dotsNames)
    assert_is_a_string(envirName)
    assert_is_environment(parentEnvir)

    envir <- new.env(parent = parentEnvir)
    invisible(lapply(seq_along(dots), function(a) {
        assign(dotsNames[[a]], eval(dots[[a]]), envir = envir)
    }))

    inform(paste("Assigning", toString(dotsNames), "as", envirName))
    assign(envirName, value = envir, envir = parentEnvir)

    invisible(objects(envir))
}
