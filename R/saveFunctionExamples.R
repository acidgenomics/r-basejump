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
        message(paste(
            "Saving", length(f), "functions from", package, "to", dir
        ))
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
            x <- .RdGetMetadata(db[[f]], kind = "examples")

            # Early return if there are no examples for that Rd file.
            if (!length(x)) {
                message(paste("Skipping", f))
                return(invisible())
            }

            # Save to an R script.
            path <- file.path(dir, paste0(f, ".R"))
            message(paste("Saving", f))
            unlink(path)
            write_lines(x = x, path = path)
            path
        },
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )

    # Return file paths of saved R scripts.
    invisible(paths)
}



# Using the unexported `RdTags()` parser internally.
.RdTags <- tools:::RdTags



# Modified version of `tools:::.Rd_get_metadata()` that keeps whitespace.
.RdGetMetadata <- function(x, kind) {
    assert_is_all_of(x, "Rd")
    x <- x[.RdTags(x) == sprintf("\\%s", kind)]
    if (!length(x)) {
        character()
    } else {
        # Coerce to character, not a character matrix.
        x <- as.character(sapply(x, as.character))
        # Strip leading and trailing whitespace, if present.
        if (x[[1L]] == "") {
            x <- x[-1L]
        }
        if (x[[length(x)]] == "") {
            x <- x[-length(x)]
        }
        x
    }
}
