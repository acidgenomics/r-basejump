#' Load Data File as Name
#'
#' @family Read Functions
#'
#' @importFrom fs path path_real
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
#'     tx2gene = "grch37Tx2gene",
#'     dir = system.file("extdata", package = "basejump")
#' )
#' glimpse(annotable)
loadDataAsName <- function(
    ...,
    dir = ".",
    envir = parent.frame()) {
    dots <- list(...)
    assert_all_are_dirs(dir)
    dir <- path_real(dir)
    assert_is_environment(envir)

    # Check for legacy mappings method, used prior to v0.1.1
    if (length(dots) == 1L & !is.null(names(dots[[1L]]))) {
        # Convert the named character vector to a named list, for consistency
        dots <- as.list(dots[[1L]])
    }

    assert_is_list(dots)
    assert_has_names(dots)
    invisible(lapply(dots, assert_is_a_string))

    # Load into a temporary environment
    fileNames <- as.character(dots)
    filePaths <- path(dir, paste0(fileNames, ".rda"))
    assert_all_are_existing_files(filePaths)
    tmpEnvir <- new.env()
    invisible(mapply(
        FUN = .safeLoad,
        file = filePaths,
        MoreArgs = list(envir = tmpEnvir),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE))
    # Check to ensure everything loaded up correctly
    assert_are_set_equal(fileNames, ls(tmpEnvir))

    # Now assign to the desired object names
    objectNames <- names(dots)
    # Check to see if any of the new names already exist in environment
    assertAllAreNonExisting(objectNames, envir = envir, inherits = FALSE)
    invisible(mapply(
        FUN = function(old, new, tmpEnvir, envir) {
        assign(
            x = new,
            value = get(old, envir = tmpEnvir, inherits = FALSE),
            envir = envir)
        },
        new = objectNames,
        old = fileNames,
        MoreArgs = list(tmpEnvir = tmpEnvir, envir = envir),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    ))

    invisible(filePaths)
}
