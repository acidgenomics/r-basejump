#' Assign Multiple Objects as an Environment
#'
#' @export
#'
#' @inheritParams dots
#' @param envirName `string`. Name of the new `environment` to create.
#' @param parentFrame `environment`. Parent `environment` where to assign the
#'   new `environment`, specified by `envirName` argument.
#'
#' @return `character`. Object names defined in the new `environment`.
#'
#' @examples
#' multiassignAsEnvir(rse_small, sce_small, envirName = "example")
#' class(example)
#' ls(example)
multiassignAsEnvir <- function(
    ...,
    envirName,
    parentFrame = parent.frame()
) {
    # FIXME Is this not working correctly in `multiassign` unit test?
    dots <- dots(...)
    assert_is_list(dots)
    names <- dots(..., character = TRUE)
    assert_is_character(names)
    assert_is_a_string(envirName)
    assert_is_environment(parentFrame)

    envir <- new.env(parent = parentFrame)
    invisible(lapply(
        X = seq_along(dots),
        FUN = function(a) {
            assign(
                x = names[[a]],
                value = eval(expr = dots[[a]], envir = parentFrame),
                envir = envir
            )
        }
    ))

    message(paste0("Assigning ", toString(names), " as ", envirName, "."))
    assign(envirName, value = envir, envir = parentFrame)

    invisible(objects(envir))
}
