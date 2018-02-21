#' Load Local Data
#'
#' Load R data (`.rda`) files from a directory using symbols rather than complete
#' file paths.
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
#' @importFrom fs path_join path_real
#'
#' @param ... Object names as symbols.
#' @param dir Output directory. Defaults to the current working directory.
#' @param envir Environment to use for assignment. Defaults to `parent.frame()`,
#'   which will assign into the calling environment.
#' @param quiet If `TRUE`, suppress any status messages and/or progress bars.
#'
#' @return Silent named character vector of file paths.
#' @export
#'
#' @examples
#' # Load the internal GRCh37 gene annotations data
#' loadData(grch37, dir = system.file("extdata", package = "basejump"))
#' glimpse(grch37)
loadData <- function(
    ...,
    dir = ".",
    envir = parent.frame(),
    quiet = FALSE) {
    assert_all_are_dirs(dir)
    dir <- path_real(dir)
    assert_is_environment(envir)
    assert_is_a_bool(quiet)

    # `dots()` method will fail here because the objects aren't present
    dots <- as.list(substitute(list(...)))[-1L]
    invisible(lapply(dots, assert_is_name))

    names <- as.character(dots)
    files <- path_join(c(dir, paste0(names, ".rda")))
    assert_all_are_existing_files(files)

    if (!isTRUE(quiet)) {
        inform(paste("Loading", toString(basename(files)), "from", dir))
    }

    objects <- mapply(
        FUN = .safeLoad,
        files,
        MoreArgs = list(envir = envir),
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
    names(objects) <- names

    assert_is_character(objects)
    invisible(objects)
}
