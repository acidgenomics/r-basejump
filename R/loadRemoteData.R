#' Load Remote Data
#'
#' Load a remote R binary file. This function is vectorized and supports
#' multiple URLs in a single call.
#'
#' @family Read Functions
#'
#' @inheritParams general
#' @inheritParams loadData
#' @param url Remote URL file path to R Data file. Supports multiple URLs
#'   passed in as a character vector.
#'
#' @return Invisible character `matrix` containing URL and tempfile paths.
#' @export
#'
#' @examples
#' loaded <- loadRemoteData(c(
#'     "http://basejump.seq.cloud/mtcars.rda",
#'     "http://basejump.seq.cloud/starwars.rda"
#' ))
#' print(loaded)
loadRemoteData <- function(url, envir = parent.frame()) {
    assert_is_character(url)
    # Check for remote URL containing `.rda` file
    assert_all_are_matching_regex(url, "^http(s)?\\://.+\\.rda$")
    assert_is_environment(envir)

    # Check to make sure the objects don't already exist
    basename(url) %>%
        gsub("\\.rda$", "", .) %>%
        assertAllAreNonExisting(envir = envir, inherits = FALSE)

    .urlToTempfile <- function(url, envir = parent.frame()) {
        assert_is_a_string(url)
        assert_is_environment(envir)
        tempfile <- tempfile()
        download.file(url = url, destfile = tempfile)
        c(url = url, tempfile = as.character(tempfile))
    }

    # Download the files to tempdir and return a character matrix of mappings
    map <- mapply(
        FUN = .urlToTempfile,
        url = url,
        MoreArgs = list(envir = envir),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    )
    map <- do.call(cbind, map)
    colnames(map) <- gsub("\\.rda$", "", basename(map["url", , drop = TRUE]))
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
