#' Save R Documentation Examples
#'
#' Parse the documentation for a function and save the working examples to an R
#' script. Note that the `f` argument is parameterized and can handle multiple
#' requests in a single call.
#'
#' @family R Documentation Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#' @param Rd `character` or `NULL`. R documentation name(s) from which to parse
#'   and save the working examples. If `NULL`, all documentation files
#'   containing examples will be saved.
#' @param package `string`. Package name.
#'
#' @return Invisible `character`. File path(s).
#'
#' @examples
#' saveRdExamples(
#'     Rd = c("do.call", "droplevels"),
#'     package = "base",
#'     dir = "XXX"
#' )
#'
#' # Clean up
#' unlink("XXX", recursive = TRUE)
saveRdExamples <- function(
    Rd,
    package,
    dir = "."
) {
    assert_is_any_of(
        x = Rd,
        classes = c("character", "NULL")
    )
    assert_is_a_string(package)
    dir <- initializeDirectory(dir)

    # Get a database of the Rd files available in the requested package.
    db <- Rd_db(package)
    names(db) <- gsub("\\.Rd", "", names(db))

    # If no function is specified, save everything.
    if (is.null(Rd)) {
        Rd <- names(db)
    }

    # Check that the requiested function(s) are valid.
    assert_is_subset(Rd, names(db))

    # Parse the Rd files and return the working examples as a character.
    list <- mapply(
        Rd = Rd,
        MoreArgs = list(
            package = package,
            dir = dir
        ),
        FUN = function(Rd, package, dir) {
            x <- tryCatch(
                expr = parseRd(db[[Rd]], tag = "examples"),
                error = function(e) character()
            )

            # Early return if there are no examples.
            if (!length(x)) {
                message(paste("Skipping", Rd))
                return(invisible())
            }

            # Save to an R script.
            path <- file.path(dir, paste0(Rd, ".R"))
            message(paste("Saving", Rd))
            unlink(path)
            write_lines(x = x, path = path)
            path
        },
        SIMPLIFY = FALSE,
        USE.NAMES = TRUE
    )

    # Coerce to character and remove NULL items.
    paths <- Filter(Negate(is.null), list)
    names <- names(paths)
    paths <- as.character(paths)
    names(paths) <- names

    message(paste(
        "Saved", length(paths), "Rd examples from", package, "to", dir
    ))

    # Return file paths of saved R scripts.
    invisible(paths)
}
