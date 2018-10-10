#' Save R Documentation Examples
#'
#' Parse the documentation for a function and save the working examples to an R
#' script. Note that the `f` argument is parameterized and can handle multiple
#' requests in a single call.
#'
#' @family Documentation Functions
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
#'     dir = "example"
#' )
#'
#' ## Clean up.
#' unlink("example", recursive = TRUE)
saveRdExamples <- function(
    Rd = NULL,  # nolint
    package,
    dir = "."
) {
    assert_is_any_of(Rd, c("character", "NULL"))
    assert_is_a_string(package)
    dir <- initializeDirectory(dir)

    # Get a database of the Rd files available in the requested package.
    db <- Rd_db(package)
    names(db) <- gsub("\\.Rd", "", names(db))

    # If no Rd file is specified, save everything in package.
    if (is.null(Rd)) {
        Rd <- names(db)  # nolint
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
        FUN = function(Rd, package, dir) {  # nolint
            x <- tryCatch(
                expr = parseRd(db[[Rd]], tag = "examples"),
                error = function(e) character()
            )

            # Early return if there are no examples.
            if (!has_length(x)) {
                message(paste0("Skipped ", Rd, "."))
                return(invisible())
            }

            # Save to an R script.
            path <- file.path(dir, paste0(Rd, ".R"))
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

    message(paste0(
        "Saved ", length(paths), " Rd examples from ", package, " to ", dir, "."
    ))

    # Return file paths of saved R scripts.
    invisible(paths)
}
