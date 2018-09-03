#' Load Data
#'
#' Load R data files from a directory using symbols rather than complete file
#' paths. Supports "`.RData`", "`.rda`", and "`.rds`" file extensions.
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
#' the file name exactly.
#'
#' @note This function is desired for interactive use and interprets object
#' names using non-standard evaluation.
#'
#' @family Read Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param ... Object names. Note that these arguments are interpreted as symbols
#'   using non-standard evaluation for convenience during interactive use, and
#'   *should not be quoted*.
#'
#' @return Invisible `character`. File paths.
#' @export
#'
#' @examples
#' loadData(
#'     example,
#'     dir = system.file("extdata", package = "basejump")
#' )
loadData <- function(
    ...,
    dir = ".",
    envir = parent.frame()
) {
    assert_is_environment(envir)
    dots <- dots(..., character = TRUE)
    files <- .listRData(dots = dots, dir = dir)
    if (any(grepl("\\.rds$", files))) {
        # R data serialized
        mapply(
            FUN = .safeLoadRDS,
            file = files,
            MoreArgs = list(envir = envir),
            SIMPLIFY = TRUE,
            USE.NAMES = FALSE
        )
    } else {
        # R data
        mapply(
            FUN = .safeLoad,
            file = files,
            MoreArgs = list(envir = envir),
            SIMPLIFY = TRUE,
            USE.NAMES = FALSE
        )
    }
    invisible(files)
}
