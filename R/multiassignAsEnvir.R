#' Assign multiple objects into a new environment
#'
#' @note Updated 2020-01-20.
#' @export
#'
#' @inheritParams acidbase::dots
#' @param envirName `character(1)`.
#'   Name of the new `environment` to create.
#' @param parentFrame `environment`.
#'   Parent `environment` where to assign the new `environment`, specified by
#'   `envirName` argument.
#'
#' @return `character`.
#' Object names defined in the new `environment`.
#'
#' @examples
#' data(data.frame, matrix, package = "AcidTest")
#' multiassignAsEnvir(data.frame, matrix, envirName = "example")
#' class(example)
#' ls(example)
multiassignAsEnvir <- function(
    ...,
    envirName,
    parentFrame = parent.frame()
) {
    dots <- dots(...)
    assert(is.list(dots))
    names <- dots(..., character = TRUE)
    assert(
        isCharacter(names),
        isString(envirName),
        is.environment(parentFrame)
    )
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
    cli_alert(sprintf(
        "Assigning {.envvar %s} as {.envvar %s}.",
        toString(names, width = 100L), envirName
    ))
    assign(envirName, value = envir, envir = parentFrame)
    invisible(objects(envir))
}
