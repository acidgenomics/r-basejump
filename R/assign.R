#' Assign and Save Data
#'
#' Assigns a new object by name to the current working environment then saves
#' the newly assigned object, specified by `dir`.
#'
#' @rdname assignAndSaveData
#' @family Assign Utilities
#'
#' @inheritParams saveData
#' @param name Desired variable name.
#' @param env Destination environment.
#'
#' @return No value.
#' @export
#'
#' @examples
#' assignAndSaveData("test", mtcars)
assignAndSaveData <- function(
    name,
    object,
    dir = "data",
    compress = TRUE,
    env = parent.frame()) {
    print(env)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    assign(name, object, envir = env)
    save(list = name,
         file = file.path(dir, paste0(name, ".rda")),
         envir = env,
         compress = compress)
}



#' Assign Multiple Objects to a New Environment
#'
#' @rdname assignIntoNewEnv
#' @family Assign Utilities
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
