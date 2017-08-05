#' Assign and Save Data
#'
#' Assigns a new object by name to the current working environment then saves
#' the newly assigned object, specified by `dir`.
#'
#' @rdname assignAndSaveData
#' @family Object Assignment Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams saveData
#' @param name Desired variable name.
#' @param env Destination environment.
#'
#' @return Assigned object name as a string.
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
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    dir <- normalizePath(dir)
    if (!identical(environmentName(env), "")) {
        message(paste("Assigning", name, "into", environmentName(env)))
    }
    assign(name, object, envir = env)
    message(paste(
        "Saving", name, "to", basename(dir)))
    save(list = name,
         file = file.path(dir, paste0(name, ".rda")),
         envir = env,
         compress = compress)
    name
}
