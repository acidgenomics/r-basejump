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
#' loadDataAsName(
#'     renamed = example,
#'     dir = system.file("extdata", package = "basejump")
#' )
#' class(renamed)
loadDataAsName <- function(
    ...,
    dir = ".",
    envir = parent.frame()
) {
    dots <- dots(..., character = TRUE)
    assert_has_names(dots)
    files <- .listRData(dots = dots, dir = dir)
    names(files) <- names(dots)
    assert_is_environment(envir)

    # Check to see if any of the new names already exist in environment
    assertAllAreNonExisting(names(dots), envir = envir, inherits = FALSE)

    if (any(grepl("\\.rds$", files))) {
        # R data serialized: assign directly
        invisible(mapply(
            name = names(files),
            file = files,
            FUN = function(name, file, envir) {
                data <- readRDS(file)
                assign(
                    x = name,
                    value = data,
                    envir = envir
                )
            },
            MoreArgs = list(envir = envir)
        ))
    } else {
        # R data: use safe loading
        safe <- new.env()
        invisible(mapply(
            FUN = .safeLoad,
            file = files,
            MoreArgs = list(envir = safe)
        ))
        assert_are_set_equal(dots, ls(safe))

        # Now assign to the desired object names
        invisible(mapply(
            FUN = function(from, to, safe, envir) {
                assign(
                    x = to,
                    value = get(from, envir = safe, inherits = FALSE),
                    envir = envir
                )
            },
            from = dots,
            to = names(dots),
            MoreArgs = list(safe = safe, envir = envir),
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
        ))
    }

    invisible(files)
}
