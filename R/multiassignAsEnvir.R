#' Assign multiple objects into a new environment
#'
#' @inheritParams brio::dots
#' @export
#'
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
#' data(data.frame, matrix, package = "acidtest")
#' multiassignAsEnvir(data.frame, matrix, envirName = "example")
#' class(example)
#' ls(example)

## Updated 2019-07-22.
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

    message(paste0("Assigning ", toString(names), " as ", envirName, "."))
    assign(envirName, value = envir, envir = parentFrame)

    invisible(objects(envir))
}
