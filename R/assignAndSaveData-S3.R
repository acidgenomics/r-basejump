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
