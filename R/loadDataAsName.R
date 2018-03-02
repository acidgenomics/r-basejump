#' Load Data File as Name
#'
#' @note This function is desired for interactive use and interprets object
#' names using non-standard evaluation.
#'
#' @family Read Functions
#'
#' @importFrom fs path path_real
#'
#' @inheritParams loadData
#'
#' @param ... Key value pairs, defining the name mappings. For example,
#'   `newName1` = `oldName1`, `newName2` = `oldName2`. Note that these
#'   arguments are interpreted using non-standard evaluation, and *should not
#'   be quoted*.
#'
#' @return Silently return named character vector of file paths.
#' @export
#'
#' @examples
#' loadDataAsName(
#'     hg19 = grch37,
#'     hg19_tx2gene = grch37Tx2gene,
#'     dir = system.file("extdata", package = "basejump")
#' )
#' glimpse(hg19)
loadDataAsName <- function(
    ...,
    dir = ".",
    envir = parent.frame()) {
    dots <- dots(..., character = TRUE)
    assert_is_character(dots)
    assert_has_names(dots)
    invisible(lapply(dots, assert_is_a_string))
    assert_all_are_dirs(dir)
    dir <- path_real(dir)
    assert_is_environment(envir)

    files <- path(dir, paste0(dots, ".rda"))
    names(files) <- names(dots)
    assert_all_are_existing_files(files)

    # Load into a temporary environment
    tmpEnvir <- new.env()
    invisible(mapply(
        FUN = .safeLoad,
        file = files,
        MoreArgs = list(envir = tmpEnvir),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE))
    assert_are_set_equal(dots, ls(tmpEnvir))

    # Now assign to the desired object names
    # Check to see if any of the new names already exist in environment
    assertAllAreNonExisting(names(dots), envir = envir, inherits = FALSE)
    invisible(mapply(
        FUN = function(old, new, tmpEnvir, envir) {
        assign(
            x = new,
            value = get(old, envir = tmpEnvir, inherits = FALSE),
            envir = envir)
        },
        new = names(dots),
        old = dots,
        MoreArgs = list(tmpEnvir = tmpEnvir, envir = envir),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    ))

    invisible(files)
}
