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
#'
#' @return Assigned object name as a string.
#' @export
#'
#' @note This function attempts to follow the same order as [base::assign()].
#'
#' @examples
#' assignAndSaveData("test", mtcars)
assignAndSaveData <- function(
    name,
    object,
    dir = "data",
    compress = TRUE) {
    envir <- parent.frame()
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    dir <- normalizePath(dir)
    file <- file.path(dir, paste0(name, ".rda"))
    names(file) <- name
    assign(name, object, envir = envir)
    message(paste("Saving", name, "to", basename(dir)))
    save(list = name,
         file = file,
         envir = envir,
         compress = compress)
    # Silently return the file path as a named character vector
    invisible(file)
}
