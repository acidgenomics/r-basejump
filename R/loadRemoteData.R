#' Load Remote Data
#'
#' Load a remote R binary file. This function is vectorized and supports
#' multiple URLs in a single call.
#'
#' @family Read Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param url Remote URL file path to R Data file. Supports multiple URLs
#'   passed in as a character vector.
#'
#' @return Invisible character `matrix` containing URL and tempfile paths.
#' @export
#'
#' @examples
#' loadRemoteData(c(
#'     "http://basejump.seq.cloud/rnaseqCounts.rda",
#'     "http://basejump.seq.cloud/singleCellCounts.rda"
#' ))
#' class(rnaseqCounts)
#' class(singleCellCounts)
loadRemoteData <- function(url, envir = parent.frame()) {
    assert_is_character(url)
    assert_all_are_matching_regex(
        x = tolower(url),
        pattern = paste0("^http(s)?\\://.+", rdataExtPattern)
    )
    assert_is_environment(envir)
    names <- gsub(rdataExtPattern, "", basename(url), ignore.case = TRUE)
    names(url) <- names

    # Check to make sure the objects don't already exist
    assertAllAreNonExisting(names, envir = envir, inherits = FALSE)

    # Download the files to tempdir and return a character matrix of mappings
    invisible(mapply(
        name = names,
        url = url,
        MoreArgs = list(envir = envir),
        FUN = function(name, url, envir) {
            data <- readFileByExtension(url)
            assign(
                x = name,
                value = data,
                envir = envir
            )
        }
    ))

    assert_all_are_existing(names, envir = envir, inherits = FALSE)
    invisible(url)
}
