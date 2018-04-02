#' Load Data
#'
#' Load R data (`.rda`) files from a directory using symbols rather than
#' complete file paths.
#'
#' [loadData()] is opinionated about the format of R data files it will accept.
#' [base::save()] allows for the saving of multiple objects into a single R data
#' file. This can later result in unexpected accidental replacement of an
#' existing object in the current environment. Additionally, since an R data
#' file internally stores the name of an object, if the file is later renamed
#' the object name will no longer match.
#'
#' To avoid any accidental replacements, [loadData()] will only load R data
#' files that contain a single object, and the internal object name must match
#' the file name exactly. These conventions match the recommendations of the
#' RStudio team, which recommends saving single objects per file.
#'
#' @note This function is desired for interactive use and interprets object
#' names using non-standard evaluation.
#'
#' @family Read Functions
#' @author Michael Steinbaugh
#'
#' @param ... Object names. Note that these arguments are interpreted using
#'   non-standard evaluation, and *should not be quoted*.
#' @param dir Output directory. Defaults to the current working directory.
#' @param envir Environment to use for assignment. Defaults to `parent.frame()`,
#'   which will assign into the calling environment.
#'
#' @return Invisible `character` containing file paths.
#' @export
#'
#' @examples
#' loaded <- loadData(
#'     mn,
#'     dir = system.file("extdata", package = "basejump")
#' )
#' print(loaded)
#' class(mn)
loadData <- function(
    ...,
    dir = ".",
    envir = parent.frame()
) {
    assert_all_are_dirs(dir)
    assert_is_a_string(dir)
    dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)
    assert_is_environment(envir)

    dots <- dots(..., character = TRUE)
    files <- normalizePath(
        path = file.path(dir, paste0(dots, ".rda")),
        winslash = "/",
        mustWork = TRUE
    )

    inform(paste("Loading", toString(basename(files)), "from", dir))
    objects <- mapply(
        FUN = .safeLoad,
        file = files,
        MoreArgs = list(envir = envir),
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
    names(objects) <- dots

    invisible(objects)
}
