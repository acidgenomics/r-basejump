#' Load Remote Data
#'
#' Load a remote R binary file. This function is vectorized and supports
#' multiple URLs in a single call.
#'
#' @family Read Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param url Remote URL file path to R data. Supports multiple URLs passed in
#'   as a character vector.
#'
#' @return Invisible named `character` containing the local object name as the
#'   name, and the remote URL as the value.
#' @export
#'
#' @examples
#' loadRemoteData(c(
#'     "http://basejump.seq.cloud/rnaseq_counts.rda",
#'     "http://basejump.seq.cloud/single_cell_counts.rda"
#' ))
#' class(rnaseq_counts)
#' class(single_cell_counts)
loadRemoteData <- function(url, envir = parent.frame()) {
    assertIsURL(url)
    if (!all(vapply(
        X = url,
        FUN = function(x) {
            grepl(rdataExtPattern, x, ignore.case = TRUE)
        },
        FUN.VALUE = logical(1L)
    ))) {
        stop(rdataError)
    }
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
