#' Load Data File as Name
#'
#' @inheritParams loadData
#'
#' @param ... Key value pairs, defining the name mappings. The argument name
#'   defines the new name of the object in the environment, whereas the value
#'   (string) denotes the original object name. For example, `newName1 =
#'   "oldName1", newName2 = "oldName2"`.
#'
#' @return Silently return named character vector of file paths.
#' @export
#'
#' @examples
#' loadDataAsName(
#'     annotable = "grch37",
#'     dir = system.file("extdata", package = "basejump")
#' )
#' glimpse(annotable)
loadDataAsName <- function(
    ...,
    dir = getwd(),
    envir = parent.frame()) {
    dots <- list(...)
    assert_all_are_dirs(dir)
    dir <- normalizePath(dir)
    assert_is_environment(envir)

    # Check for legacy mappings method, used prior to v0.1.1
    if (length(dots) == 1L & !is.null(names(dots[[1L]]))) {
        # Convert the named character vector to a named list, for consistency
        dots <- as.list(dots[[1L]])
    }

    assert_is_list(dots)
    assert_has_names(dots)
    invisible(lapply(dots, assert_is_a_string))

    fileNames <- as.character(dots)
    objectNames <- names(dots)

    files <- file.path(dir, paste0(fileNames, ".rda"))
    names(files) <- objectNames

    # Check to see if any of the new names already exist in environment
    assertAllAreNonExisting(objectNames, envir = envir, inherits = FALSE)

    tmpEnvir <- new.env()
    mapply(
        .safeLoad,
        file = files,
        MoreArgs = list(envir = tmpEnvir),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    )
    assert_are_identical(fileNames, ls(tmpEnvir))

    invisible(lapply(seq_along(fileNames), function(a) {
        assign(
            x = objectNames[[a]],
            value = get(fileNames[[a]], envir = tmpEnvir, inherits = FALSE),
            envir = envir
        )
    }))

    invisible(files)
}
