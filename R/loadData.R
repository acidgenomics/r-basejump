#' Load Data
#'
#' Load R data (`.rda`) files from a directory using symbols rather than
#' complete file paths.
#'
#' @details
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
#'
#' @importFrom fs path_join path_real
#'
#' @param ... Object names. Note that these arguments are interpreted using
#'   non-standard evaluation, and *should not be quoted*.
#' @param dir Output directory. Defaults to the current working directory.
#' @param envir Environment to use for assignment. Defaults to `parent.frame()`,
#'   which will assign into the calling environment.
#'
#' @return Silent named character vector of file paths.
#' @export
#'
#' @examples
#' loadData(
#'     makeNames,
#'     dir = system.file("extdata", package = "basejump")
#' )
#' class(makeNames)
loadData <- function(
    ...,
    dir = ".",
    envir = parent.frame()
) {
    assert_all_are_dirs(dir)
    dir <- path_real(dir)
    assert_is_environment(envir)

    # `dots()` method will fail here because the objects aren't present
    dots <- dots(..., character = TRUE)
    files <- path_join(c(dir, paste0(dots, ".rda")))
    assert_all_are_existing_files(files)

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
