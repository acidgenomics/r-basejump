#' Load Remote Data
#'
#' Load a remote R binary file. This function is vectorized and supports
#' multiple URLs in a single call.
#'
#' @export
#'
#' @inheritParams basejump.globals::params
#' @param url `character`. Remote URL file path(s) to R data.
#'
#' @return Invisible named `character`. Local object name as the name, and the
#'   remote URL as the value.
#'
#' @examples
#' cacheURL <- basejump.globals::basejumpCacheURL
#' url <- file.path(
#'     cacheURL,
#'     paste0(c("rnaseq_counts", "single_cell_counts"), ".rds")
#' )
#' print(url)
#' x <- loadRemoteData(url)
#' print(x)
loadRemoteData <- function(url, envir = parent.frame()) {
    assert_that(has_internet())
    assertAllAreURL(url)
    if (!all(vapply(
        X = url,
        FUN = function(x) {
            grepl(rdataExtPattern, x, ignore.case = TRUE)
        },
        FUN.VALUE = logical(1L)
    ))) {
        stop(rdataLoadError, call. = FALSE)
    }
    assert_is_environment(envir)
    names <- gsub(rdataExtPattern, "", basename(url), ignore.case = TRUE)
    names(url) <- names

    # Check to make sure the objects don't already exist.
    assertAllAreNonExisting(names, envir = envir, inherits = FALSE)

    # Download the files to tempdir and return a character matrix of mappings.
    invisible(mapply(
        name = names,
        url = url,
        MoreArgs = list(envir = envir),
        FUN = function(name, url, envir) {
            data <- import(url)
            assign(x = name, value = data, envir = envir)
        }
    ))

    assert_all_are_existing(names, envir = envir, inherits = FALSE)
    invisible(url)
}



# Internal =====================================================================
.listRData <- function(
    names,
    dir = "."
) {
    assert_is_character(names)
    assert_all_are_dirs(dir)
    assert_is_a_string(dir)
    dir <- realpath(dir)
    files <- vapply(
        X = names,
        FUN = function(name) {
            files <- list.files(
                path = dir,
                pattern = paste0("^", name, rdataExtPattern),
                full.names = TRUE,
                ignore.case = TRUE
            )
            # Add error checking here.
            if (!has_length(files)) {
                stop(paste0(
                    name, " is missing.\n",
                    rdataLoadError
                ), call. = FALSE)
            } else if (length(files) > 1L) {
                stop(paste0(
                    name, " is not unique on disk.\n",
                    rdataLoadError
                ), call. = FALSE)
            }
            files
        },
        FUN.VALUE = character(1L),
        USE.NAMES = TRUE
    )
    message(paste("Loading", toString(basename(files)), "from", dir))
    files
}
