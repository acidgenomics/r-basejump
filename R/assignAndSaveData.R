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
#'
#' @param name Desired variable name.
#' @param envir Environment to use for assignment. Defaults to [parent.frame()],
#'   the calling environment.
#'
#' @return Silent named character vector of file path.
#' @export
#'
#' @note This function attempts to follow the same order as [base::assign()].
#'
#' @examples
#' assignAndSaveData(name = "test", object = mtcars)
#'
#' unlink("test.rda")
assignAndSaveData <- function(
    name,
    object,
    dir = getwd(),
    compress = TRUE,
    envir = parent.frame(),
    quiet = FALSE) {
    assert_is_a_string(name)
    assert_is_not_null(object)
    dir <- initializeDirectory(dir)
    .assert_compress(compress)
    assert_is_environment(envir)
    assert_is_a_bool(quiet)

    file <- file.path(dir, paste0(name, ".rda"))
    names(file) <- name

    assign(name, object, envir = envir)
    if (!isTRUE(quiet)) {
        inform(paste("Saving", name, "to", dir))
    }

    save(
        list = name,
        file = file,
        envir = envir,
        compress = compress
    )

    invisible(file)
}
