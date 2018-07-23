#' Assign and Save Data
#'
#' Assigns a new object by name to the current working environment then saves
#' the newly assigned object, specified by the "`dir`" argument.
#'
#' @family Write Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams saveData
#' @inheritParams general
#' @param name `string`. Desired variable name.
#' @param envir `environment`. Environment to use for assignment. Defaults to
#'   [parent.frame()], the calling environment.
#'
#' @return Invisible named `character` containing file paths.
#' @export
#'
#' @note This function attempts to follow the same order as [base::assign()].
#'
#' @examples
#' assignAndSaveData(name = "example", object = rnaseq_counts)
#' exists("example", inherits = FALSE)
#' file.exists("example.rda")
#'
#' # Clean up
#' rm(example)
#' unlink("example.rda")
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
    assign(name, object, envir = envir)

    # Save
    message(paste("Saving", name, "to", dir))
    file <- file.path(dir, paste0(name, ".rda"))
    save(
        list = name,
        file = file,
        envir = envir,
        compress = compress
    )
    file <- normalizePath(file, winslash = "/", mustWork = TRUE)
    names(file) <- name

    invisible(file)
}
