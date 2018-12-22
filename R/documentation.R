#' Parse R documentation
#'
#' Modified version of `tools:::.Rd_get_metadata` that keeps whitespace and
#' returns `character` instead of `matrix`.
#'
#' @export
#'
#' @param object `Rd`.
#'   R documentation, returned from `tools::Rd_db`.
#' @param tag `character(1)`.
#'   Desired metadata type.
#'
#'   These types are supported:
#'
#'   - "`title`".
#'   - "`description`".
#'   - "`usage`".
#'   - "`arguments`".
#'   - "`value`".
#'   - "`references`".
#'   - "`seealso`".
#'   - "`examples`".
#'
#' @seealso `tools::Rd_db`.
#'
#' @examples
#' db <- tools::Rd_db("base")
#' head(names(db))
#' Rd <- db[["nrow.Rd"]]
#' class(Rd)
#' summary(Rd)
#' RdTags(Rd)
#' examples <- parseRd(Rd, tag = "examples")
#' print(examples)
parseRd <- function(object, tag) {
    assert(
        is(object, "Rd"),
        isString(tag)
    )

    tags <- RdTags(object)
    assert(isSubset(x = tag, y = tags))

    # Get the metadata that matches the requested tag.
    data <- object[tags == tag]
    data <- unlist(data)

    # Strip trailing newlines and superfluous whitespace.
    data <- trimws(data, which = "right")

    # Strip leading and trailing carriage returns, if present.
    if (data[[1L]] == "") {
        data <- data[-1L]
    }
    if (data[[length(data)]] == "") {
        data <- data[-length(data)]
    }

    data
}



#' R documentation tags
#'
#' Modified version of the unexported `tools:::RdTags` function.
#'
#' @export
#'
#' @inheritParams parseRd
#'
#' @examples
#' db <- tools::Rd_db("base")
#' Rd <- db[["nrow.Rd"]]
#' RdTags(Rd)
RdTags <- function(object) {  # nolint
    assert(is(object, "Rd"))
    tags <- vapply(
        X = object,
        FUN = attr,
        FUN.VALUE = character(1L),
        "Rd_tag"
    )
    if (length(tags) == 0L) {
        tags <- character()
    } else {
        # Remove the leading "\\" backslashes.
        tags <- gsub("^\\\\", "", tags)
    }
    tags
}



#' Save R documentation examples
#'
#' Parse the documentation for a function and save the working examples to an R
#' script. Note that the `f` argument is parameterized and can handle multiple
#' requests in a single call.
#'
#' @export
#'
#' @inheritParams params
#' @param Rd `character` or `NULL`.
#'   R documentation name(s) from which to parse and save the working examples.
#'   If `NULL`, all documentation files containing examples will be saved.
#' @param package `character(1)`.
#'   Package name.
#'
#' @return Invisible `character`.
#' File path(s).
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
    assert(
        isAny(Rd, classes = c("character", "NULL")),
        isString(package)
    )
    dir <- initDir(dir)

    # Get a database of the Rd files available in the requested package.
    db <- Rd_db(package)
    names(db) <- gsub("\\.Rd", "", names(db))

    # If no Rd file is specified, save everything in package.
    if (is.null(Rd)) {
        Rd <- names(db)  # nolint
    }

    # Check that the requiested function(s) are valid.
    assert(isSubset(Rd, names(db)))

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
            if (length(x) == 0L) {
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



#' R documentation table
#'
#' @param df `data.frame`.
#'
#' @return Console output.
#' @export
#'
#' @seealso http://r-pkgs.had.co.nz/man.html
tabular <- function(df) {
    assert(is.data.frame(df))
    align <- function(x) {
        if (is.numeric(x)) {
            "r"
        } else {
            "l"
        }
    }
    align <- vapply(df, align, character(1L))
    cols <- lapply(df, format)
    contents <- do.call(
        what = "paste",
        args = c(cols, list(sep = " \\tab ", collapse = "\\cr\n  "))
    )
    out <- paste(
        "\\tabular{",
        paste(align, collapse = ""),
        "}{\n  ",
        contents,
        "\n}\n",
        sep = ""
    )
    cat(out)
}
