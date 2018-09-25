#' Assign and Save Data
#'
#' Assigns a new object by name to the current working environment then saves
#' the newly assigned object, specified by the "`dir`" argument.
#'
#' @family Write Functions
#' @author Michael Steinbaugh
#' @include saveData.R
#' @export
#'
#' @inheritParams saveData
#' @inheritParams general
#' @param name `string`. Desired variable name.
#' @param envir `environment`. Environment to use for assignment. Defaults to
#'   [parent.frame()], the calling environment.
#'
#' @return Invisible named `string`. File path.
#'
#' @note This function attempts to follow the same order as [base::assign()].
#'
#' @examples
#' assignAndSaveData(name = "example", object = rse_small)
#' exists("example", inherits = FALSE)
#' file.exists("example.rda")
#'
#' # Clean up
#' rm(example)
#' unlink("example.rda")
assignAndSaveData <- function(
    name,
    object,
    envir = parent.frame()
) {
    assert_is_a_string(name)
    assert_is_not_null(object)
    dir <- initializeDirectory(dir)
    assertFormalCompress(compress)
    assert_is_environment(envir)

    # Assign data.
    assign(x = name, value = object, envir = envir)
    assign(x = name, value = object)

    # Save data.
    args <- list(
        as.name(name),
        dir = dir,
        ext = ext,
        overwrite = overwrite,
        compress = compress
    )
    file <- do.call(what = saveData, args = args)

    invisible(file)
}

# Assign the formals.
f1 <- formals(assignAndSaveData)
f2 <- formals(saveData)
f2 <- f2[setdiff(names(f2), "...")]
f <- c(f1, f2)
formals(assignAndSaveData) <- f
