#' Assign Multiple Objects as an Environment
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams dots
#' @param envirName `character` string denoting the name of the new
#'   `environment` that will be created.
#' @param parentEnvir Parent `environment` where to assign the new
#'   `environment`, defined by `envirName`.
#'
#' @return `character` vector of object names in the new `environment`.
#' @export
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
