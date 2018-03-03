#' Assign and Save Data
#'
#' Assigns a new object by name to the current working environment then saves
#' the newly assigned object, specified by `dir`.
#'
#' @importFrom fs path_join
#'
#' @family Write Functions
#'
#' @inheritParams general
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
#' exists("test", inherits = FALSE)
#' file_exists("test.rda")
#'
#' # Clean up
#' rm(test)
#' file_delete("test.rda")
assignAndSaveData <- function(
    name,
    object,
    dir = ".",
    compress = TRUE,
    envir = parent.frame()
) {
    assert_is_a_string(name)
    assert_is_not_null(object)
    dir <- initializeDirectory(dir)
    assertFormalCompress(compress)
    assert_is_environment(envir)

    # Assign
    file <- path_join(c(dir, paste0(name, ".rda")))
    names(file) <- name
    assign(name, object, envir = envir)

    # Save
    inform(paste("Saving", name, "to", dir))
    save(
        list = name,
        file = file,
        envir = envir,
        compress = compress
    )

    invisible(file)
}
