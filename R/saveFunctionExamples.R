#' Save Functions Examples
#'
#' Parse the documentation for a function and save the working examples to an
#' R script. Note that the `fun` argument is parameterized and can handle
#' multiple requests in a single call.
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#' @param fun `character`. Function(s) from which to parse and save the working
#'   examples.
#' @param package `string`. Package name.
#'
#' @return `character`. Invisible file path(s).
#'
#' @examples
#' saveFunctionExamples(
#'     fun = c("do.call", "droplevels"),
#'     package = "base",
#'     dir = "XXX"
#' )
#'
#' # Clean up
#' unlink("XXX", recursive = TRUE)
saveFunctionExamples <- function(
    fun,
    package,
    dir = "."
) {
    assert_is_character(fun)
    assert_is_a_string(package)
    dir <- initializeDirectory(dir)

    # Get a database of the Rd files available in the requested package.
    db <- Rd_db(package)
    names(db) <- gsub("\\.Rd", "", names(db))
    assert_is_subset(fun, names(db))

    # Parse the Rd files and return the working examples as a character.
    paths <- mapply(
        fun = fun,
        MoreArgs = list(
            package = package,
            dir = dir
        ),
        FUN = function(fun, package, dir) {
            # Is there an exported function we can use instead here?
            x <- tools:::.Rd_get_metadata(
                x = db[[fun]],
                kind = "examples"
            )
            x <- as.character(x)

            # Strip leading and trailing whitespace, if present.
            if (x[[1L]] == "") {
                x <- x[-1L]
            }
            if (x[[length(x)]] == "") {
                x <- x[-length(x)]
            }

            # Save to an R script.
            path <- file.path(dir, paste0(fun, ".R"))
            message(paste("Saving examples to", path))
            write_lines(x = x, path = path)
            path
        },
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )

    # Return file paths of saved R scripts.
    invisible(paths)
}
