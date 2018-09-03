#' Save Functions Examples
#'
#' Parse the documentation for a function and save the working examples to an R
#' script. Note that the `f` argument is parameterized and can handle multiple
#' requests in a single call.
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#' @param f `character` or `NULL`. Function(s) from which to parse and save the
#'   working examples. If `NULL`, all functions will be saved.
#' @param package `string`. Package name.
#'
#' @return Invisible `character`. File path(s).
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
    f,
    package,
    dir = "."
) {
    assert_is_any_of(
        x = f,
        classes = c("character", "NULL")
    )
    assert_is_a_string(package)
    dir <- initializeDirectory(dir)

    # Get a database of the Rd files available in the requested package.
    db <- Rd_db(package)
    names(db) <- gsub("\\.Rd", "", names(db))

    # If no function is specified, save everything.
    if (is.null(f)) {
        f <- names(db)
        message(paste("Saving", length(f), "functions from", package))
    }

    # Check that the requiested function(s) are valid.
    assert_is_subset(f, names(db))

    # Parse the Rd files and return the working examples as a character.
    paths <- mapply(
        f = f,
        MoreArgs = list(
            package = package,
            dir = dir
        ),
        FUN = function(f, package, dir) {
            # Is there an exported function we can use instead here?
            x <- tools:::.Rd_get_metadata(
                x = db[[f]],
                kind = "examples"
            )

            if (!length(x)) {
                print("Early return")
                return(invisible())
            }

            x <- as.character(x)

            # Strip leading and trailing whitespace, if present.
            if (x[[1L]] == "") {
                x <- x[-1L]
            }
            if (x[[length(x)]] == "") {
                x <- x[-length(x)]
            }

            # Save to an R script.
            path <- file.path(dir, paste0(f, ".R"))
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
