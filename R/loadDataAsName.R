#' Load Data as Name
#'
#' @note This function is intended for interactive use and interprets object
#'   names using non-standard evaluation.
#'
#' @family Read Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams loadData
#' @param ... Key value pairs, defining the name mappings. For example,
#'   `newName1` = `oldName1`, `newName2` = `oldName2`. Note that these
#'   arguments are interpreted using non-standard evaluation, and *should not
#'   be quoted*.
#'
#' @return Invisible named `character` containing file paths.
#' @export
#'
#' @examples
#' loaded <- loadDataAsName(
#'     makeNames = mn,
#'     dir = system.file("extdata", package = "basejump")
#' )
#' print(loaded)
#' class(makeNames)
loadDataAsName <- function(
    ...,
    dir = ".",
    envir = parent.frame()
) {
    dots <- dots(..., character = TRUE)
    assert_is_character(dots)
    assert_has_names(dots)
    invisible(lapply(dots, assert_is_a_string))
    assert_all_are_dirs(dir)
    assert_is_a_string(dir)
    dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)
    assert_is_environment(envir)

    files <- file.path(dir, paste0(dots, ".rda"))
    names(files) <- names(dots)
    assert_all_are_existing_files(files)

    # Load into a temporary environment
    tmpEnvir <- new.env()
    invisible(mapply(
        FUN = .safeLoad,
        file = files,
        MoreArgs = list(envir = tmpEnvir),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    ))
    assert_are_set_equal(dots, ls(tmpEnvir))

    # Now assign to the desired object names
    # Check to see if any of the new names already exist in environment
    assertAllAreNonExisting(names(dots), envir = envir, inherits = FALSE)
    invisible(mapply(
        FUN = function(old, new, tmpEnvir, envir) {
            assign(
                x = new,
                value = get(old, envir = tmpEnvir, inherits = FALSE),
                envir = envir
            )
        },
        new = names(dots),
        old = dots,
        MoreArgs = list(tmpEnvir = tmpEnvir, envir = envir),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    ))

    invisible(files)
}
