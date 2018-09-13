#' Assign Multiple Objects as an Environment
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams dots
#' @param envirName `string`. Name of the new `environment` to create.
#' @param parentEnvir `environment`. Parent `environment` where to assign the
#'   new `environment`, specified by `envirName` argument.
#'
#' @return `character`. Object names defined in the new `environment`.
#'
#' @examples
#' multiassignAsEnvir(rnaseq_counts, single_cell_counts, envirName = "example")
#' class(example)
#' ls(example)
multiassignAsEnvir <- function(
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

    message(paste("Assigning", toString(dotsNames), "as", envirName))
    assign(envirName, value = envir, envir = parentEnvir)

    invisible(objects(envir))
}
