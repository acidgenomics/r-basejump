#' Load Remote Data
#'
#' Load a remote R binary file.
#'
#' @family Data Import and Project Utilities
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom utils download.file
#'
#' @inheritParams general
#' @inheritParams loadData
#'
#' @param url Remote URL file path to R Data file. Supports multiple URLs
#'   passed in as a character vector.
#'
#' @return Silently return a character matrix containing URL and tempfile paths.
#' @export
#'
#' @examples
#' objects <- loadRemoteData(c(
#'     "http://basejump.seq.cloud/mtcars.rda",
#'     "http://basejump.seq.cloud/starwars.rda"
#' ))
#' objects
loadRemoteData <- function(
    url,
    envir = parent.frame(),
    quiet = FALSE) {
    assert_is_character(url)
    # Check for remote URL containing `.rda` file
    assert_all_are_matching_regex(url, "^http(s)?\\://.+\\.rda$")
    assert_is_environment(envir)
    assert_is_a_bool(quiet)

    # Check to make sure the objects don't already exist
    basename(url) %>%
        file_path_sans_ext() %>%
        assert_all_are_non_existing(envir = envir, inherits = FALSE)

    .urlToTempfile <- function(url, envir = parent.frame(), quiet = FALSE) {
        assert_is_a_string(url)
        assert_is_environment(envir)
        assert_is_a_bool(quiet)
        tempfile <- tempfile()
        download.file(
            url = url,
            destfile = tempfile,
            quiet = quiet)
        c(url = url, tempfile = tempfile)
    }

    # Download the files to tempdir and return a character matrix of mappings
    map <- mapply(
        FUN = .urlToTempfile,
        url = url,
        MoreArgs = list(envir = envir, quiet = quiet),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    )
    map <- do.call(cbind, map)
    colnames(map) <- file_path_sans_ext(basename(map["url", , drop = TRUE]))
    assert_is_matrix(map)

    # Now we're ready to load safely from the tempdir
    files <- map["tempfile", , drop = FALSE]
    names <- colnames(map)
    objects <- mapply(
        FUN = .safeLoad,
        file = files,
        name = names,
        MoreArgs = list(envir = envir),
        SIMPLIFY = FALSE,
        USE.NAMES = TRUE
    )
    objects <- do.call(cbind, objects)
    colnames(objects) <- names

    return <- map[, colnames(objects), drop = FALSE]
    assert_is_matrix(return)
    invisible(return)
}
