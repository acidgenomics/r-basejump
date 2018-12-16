#' Make syntactically valid names
#'
#' For `atomic` vectors, these functions will sanitize the values. Otherwise,
#' they will set `names`, `rownames`, and/or `colnames` without
#' modification of the values.
#'
#' `dotted.case` support is provided for matching against base R parameters.
#' However, it is recommended to avoid using it for variable assignments into an
#' `environment`, as that can introduce conflicts with base functions.
#'
#' @note `makeNames` sanitizes names using underscores instead of dots, the
#' convention used by `make.names`.
#'
#' @name makeNames
#' @inheritParams params
#'
#' @param object `character` or class supporting `names`.
#' @param rownames `logical(1)`.
#'   Apply sanitization on row names. This is not generally recommended by
#'   default, since rownames commonly contain gene identifiers that should not
#'   be modified.
#' @param colnames `logical(1)`.
#'   Apply sanitization on column names. This is generally recommended by
#'   default.
#' @param strict `logical(1)`.
#'   Enforce strict name sanitization. When `TRUE`, this does not allow the
#'   return of any capitalized acronyms. "RNA" will become "Rna", for example.
#'
#' @return Modified object.
#' Contains syntatically valid names. For objects supporting `names`, the
#' underlying data returns unchanged.
#'
#' @seealso
#' - `make.names`.
#' - [janitor](https://cran.r-project.org/package=janitor) contains a number of
#'   useful functions that provide similar sanitization support, but isn't
#'   designed to work natively with [Bioconductor](https://bioconductor.org) and
#'   S4 object classes.
#' - [lettercase](https://cran.r-project.org/package=lettercase).
#' - [snakecase](https://tazinho.github.io/snakecase).
#'
#' @examples
#' loadRemoteData(url = file.path(basejumpCacheURL, "mn.rda"))
#'
#' ## character ====
#' x <- mn$character
#' print(x)
#' camel(x)
#' dotted(x)
#' snake(x)
#' upperCamel(x)
#' makeNames(x)
#'
#' x <- mn$namedCharacter
#' print(x)
#' camel(x)
#' dotted(x)
#' snake(x)
#' upperCamel(x)
#' makeNames(x)
#'
#' ## factor ====
#' x <- mn$factor
#' print(x)
#' camel(x)
#' dotted(x)
#' snake(x)
#' upperCamel(x)
#' makeNames(x)
#'
#' ## data.frame ====
#' x <- datasets::USArrests
#' dimnames(x)
#' camel(x, rownames = TRUE, colnames = TRUE) %>% dimnames()
#' dotted(x, rownames = TRUE, colnames = TRUE) %>% dimnames()
#' snake(x, rownames = TRUE, colnames = TRUE) %>% dimnames()
#' upperCamel(x, rownames = TRUE, colnames = TRUE) %>% dimnames()
#'
#' ## list ====
#' x <- mn$list
#' print(x)
#' camel(x) %>% names()
#' dotted(x) %>% names()
#' snake(x) %>% names()
#' upperCamel(x) %>% names()
NULL



# Constructors =================================================================
# `stringr::str_replace_all` is an alternate approach that uses `regex`.
# https://stringr.tidyverse.org/articles/regular-expressions.html
.sanitizeAcronyms <- function(object) {
    assert(is.atomic(object))
    object %>%
        as.character() %>%
        # Sanitize "id" variants (e.g. "Id" to "ID").
        gsub(
            pattern = "\\b(id)\\b",
            replacement = "ID",
            x = .,
            ignore.case = TRUE
        ) %>%
        # Sanitize plurarlized acronyms (e.g. "UMIs" to "UMIS").
        gsub(
            pattern = "\\b([A-Z0-9]+)s\\b",
            replacement = "\\1S",
            x = .
        ) %>%
        # Sanitize mixed case concentrations (e.g. "10nM" to "10NM").
        gsub(
            pattern = "\\b([[:digit:]]+?[mnu]M)\\b",
            replacement = "\\U\\1",
            x = .,
            perl = TRUE
        ) %>%
        # Sanitize mixed case RNA types.
        gsub(
            pattern = "\\b([mi|nc|pi|r]RNA)\\b",
            replacement = "\\U\\1",
            x = .,
            perl = TRUE
        ) %>%
        # Handle RNA interference.
        gsub(
            pattern = "\\b(RNAi)\\b",
            replacement = "RNAI",
            x = .
        )
}



# makeNames ====================================================================
#' @rdname makeNames
#' @inheritParams base::make.names
#' @export
makeNames <- function(names, unique = TRUE) {
    assert(
        is.atomic(names),
        isFlag(unique)
    )
    names <- as.character(names)
    names <- make.names(names, unique = unique)
    names <- gsub("\\.", "_", names)
    names
}



#' @rdname makeNames
#' @export
makeDimnames <- function(object) {
    assert(hasDimnames(object))
    # Row names.
    if (hasRownames(object)) {
        rownames(object) <- makeNames(rownames(object), unique = TRUE)
    }
    # Column names.
    if (hasColnames(object)) {
        colnames(object) <- makeNames(colnames(object), unique = TRUE)
    }
    object
}



# character ====================================================================
camel.character <-  # nolint
    function(
        object,
        format = c("lower", "upper"),
        strict = FALSE
    ) {
        object <- dotted(object)
        format <- match.arg(format)
        assert(isFlag(strict))

        # Simplify mixed case acronyms in strict mode.
        if (isTRUE(strict)) {
            object <- tolower(object)
        }

        # lowerCamelCase or UpperCamelCase.
        if (format == "lower") {
            # lowerCamelCase
            # Coerce first word to lower.
            object <- gsub(
                pattern = "^(\\w+)\\b",
                replacement = "\\L\\1",
                x = object,
                perl = TRUE
            )
        } else if (format == "upper") {
            # UpperCamelCase
            # Capitalize the first letter.
            object <- gsub(
                pattern = "^([a-z])",
                replacement = "\\U\\1",
                x = object,
                perl = TRUE
            )
        }

        # Remove dots in between numbers following a letter.
        object <- gsub("([[:alpha:]])\\.([[:digit:]])", "\\1\\2", object)

        # First letter of second word must be capitalized.
        object <- gsub("\\.([[:alpha:]])", "\\U\\1", object, perl = TRUE)

        # Remaining dots should be sanitized with "X" character.
        pattern <- "\\."
        if (any(grepl(pattern, object))) {
            if (format == "lower") {
                replacement <- "x"
            } else if (format == "upper") {
                replacement <- "X"
            }
            object <- gsub(pattern, replacement, object)
        }

        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("character"),
    definition = function(object, strict = FALSE) {
        if (hasNames(object)) {
            names <- camel.character(names(object), strict = strict)
        } else {
            names <- NULL
        }
        object <- camel.character(object, strict = strict)
        names(object) <- names
        object
    }
)



# Dotted case is the internal method used by camel and snake.
dotted.character <-  # nolint
    function(object) {
        object %>%
            as.character() %>%
            # Handle "+" as a special case. Spell out as "plus".
            gsub("\\+", ".plus.", .) %>%
            # Handle "%" as a special case. Spell out as "percent".
            gsub("%", "percent", .) %>%
            # Strip comma delims in between numbers (e.g. 1,000,000).
            gsub("(\\d),(\\d)", "\\1\\2", .) %>%
            make.names(unique = FALSE, allow_ = FALSE) %>%
            # Ensure all non-alphanumeric characters get coerced to periods.
            gsub("[^[:alnum:]]", ".", .) %>%
            # Combine multiple dots.
            gsub("[\\.]+", ".", .) %>%
            # Strip leading or trailing dots.
            gsub("(^\\.|\\.$)", "", .) %>%
            # Coerce `"NA"` back to `NA` after `make.names`.
            sanitizeNA() %>%
            # Standardize any mixed case acronyms.
            .sanitizeAcronyms() %>%
            # Establish word boundaries for camelCase acronyms
            # (e.g. `worfdbHTMLRemap` -> `worfdb.HTML.remap`).
            # Acronym following a word.
            gsub("([a-z])([A-Z])", "\\1.\\2", .) %>%
            # Word following an acronym.
            gsub("([A-Z0-9])([A-Z])([a-z])", "\\1.\\2\\3", .)
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("character"),
    definition = function(object) {
        if (hasNames(object)) {
            names <- dotted.character(names(object))
        } else {
            names <- NULL
        }
        object <- dotted.character(object)
        names(object) <- names
        object
    }
)



snake.character <-  # nolint
    function(object) {
        object %>%
            dotted() %>%
            tolower() %>%
            gsub("\\.", "_", .)
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("character"),
    definition = function(object) {
        if (hasNames(object)) {
            names <- snake.character(names(object))
        } else {
            names <- NULL
        }
        object <- snake.character(object)
        names(object) <- names
        object
    }
)



upperCamel.character <-  # nolint
    function(object, strict = FALSE) {
        camel.character(object, format = "upper", strict = strict)
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("character"),
    definition = function(object, strict = FALSE) {
        if (hasNames(object)) {
            names <- upperCamel.character(names(object), strict = strict)
        } else {
            names <- NULL
        }
        object <- upperCamel.character(object, strict = strict)
        names(object) <- names
        object
    }
)



# factor =======================================================================
camel.factor <-  # nolint
    function(object, strict = FALSE) {
        names <- names(object)
        object <- object %>%
            as.character() %>%
            camel(strict = strict) %>%
            as.factor()
        names(object) <- camel(names, strict = strict)
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("factor"),
    definition = camel.factor
)



dotted.factor <-  # nolint
    function(object) {
        names <- names(object)
        object <- object %>%
            as.character() %>%
            dotted() %>%
            as.factor()
        names(object) <- dotted(names)
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("factor"),
    definition = dotted.factor
)



snake.factor <-  # nolint
    function(object) {
        names <- names(object)
        object <- object %>%
            as.character() %>%
            snake() %>%
            as.factor()
        names(object) <- snake(names)
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("factor"),
    definition = snake.factor
)



upperCamel.factor <-  # nolint
    function(object, strict = FALSE) {
        names <- names(object)
        object <- object %>%
            as.character() %>%
            upperCamel(strict = strict) %>%
            as.factor()
        names(object) <- upperCamel(names, strict = strict)
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("factor"),
    definition = upperCamel.factor
)



# atomic =======================================================================
camel.atomic <-  # nolint
    function(object, strict = FALSE) {
        if (hasNames(object)) {
            names(object) <- camel.character(names(object), strict = strict)
        }
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("atomic"),
    definition = camel.atomic
)



dotted.atomic <-  # nolint
    function(object) {
        if (hasNames(object)) {
            names(object) <- dotted.character(names(object))
        }
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("atomic"),
    definition = dotted.atomic
)



snake.atomic <-  # nolint
    function(object) {
        if (hasNames(object)) {
            names(object) <- snake.character(names(object))
        }
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("atomic"),
    definition = snake.atomic
)



upperCamel.atomic <-  # nolint
    function(object, strict = FALSE) {
        if (hasNames(object)) {
            names(object) <-
                upperCamel.character(names(object), strict = strict)
        }
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("atomic"),
    definition = upperCamel.atomic
)



# list =========================================================================
camel.list <-  # nolint
    camel.atomic

#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("list"),
    definition = camel.list
)



dotted.list <-  # nolint
    dotted.atomic

#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("list"),
    definition = dotted.list
)



snake.list <-  # nolint
    snake.atomic

#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("list"),
    definition = snake.list
)



upperCamel.list <-  # nolint
    upperCamel.atomic

#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("list"),
    definition = upperCamel.list
)



# List =========================================================================
#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("List"),
    definition = camel.list
)



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("List"),
    definition = dotted.list
)



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("List"),
    definition = snake.list
)



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("List"),
    definition = upperCamel.list
)



# matrix =======================================================================
camel.matrix <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = FALSE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- camel(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- camel(colnames(object), strict = strict)
        }
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("matrix"),
    definition = camel.matrix
)



dotted.matrix <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- dotted.character(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- dotted.character(colnames(object))
        }
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("matrix"),
    definition = dotted.matrix
)



snake.matrix <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- snake.character(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- snake.character(colnames(object))
        }
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("matrix"),
    definition = snake.matrix
)



upperCamel.matrix <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = FALSE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <-
                upperCamel.character(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <-
                upperCamel.character(colnames(object), strict = strict)
        }
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("matrix"),
    definition = upperCamel.matrix
)



# data.frame ===================================================================
camel.data.frame <-  # nolint
    camel.matrix

#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("data.frame"),
    definition = camel.data.frame
)



dotted.data.frame <-  # nolint
    dotted.matrix

#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("data.frame"),
    definition = dotted.data.frame
)



snake.data.frame <-  # nolint
    snake.matrix

#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("data.frame"),
    definition = snake.data.frame
)



upperCamel.data.frame <-  # nolint
    upperCamel.matrix

#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("data.frame"),
    definition = upperCamel.data.frame
)



# DataFrame ====================================================================
camel.DataFrame <-  # nolint
    camel.data.frame

#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("DataFrame"),
    definition = camel.DataFrame
)



dotted.DataFrame <-  # nolint
    dotted.data.frame

#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("DataFrame"),
    definition = dotted.DataFrame
)



snake.DataFrame <-  # nolint
    snake.data.frame

#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("DataFrame"),
    definition = snake.DataFrame
)



upperCamel.DataFrame <-  # nolint
    upperCamel.data.frame

#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("DataFrame"),
    definition = upperCamel.DataFrame
)



# GRanges ======================================================================
camel.GRanges <-  # nolint
    function(object, strict = FALSE) {
        colnames(mcols(object)) <- camel(
            object = colnames(mcols(object)),
            strict = strict
        )
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("GRanges"),
    definition = camel.GRanges
)



dotted.GRanges <-  # nolint
    function(object) {
        colnames(mcols(object)) <- dotted(
            object = colnames(mcols(object))
        )
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("GRanges"),
    definition = dotted.GRanges
)



snake.GRanges <-  # nolint
    function(object) {
        colnames(mcols(object)) <- snake(
            object = colnames(mcols(object))
        )
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("GRanges"),
    definition = snake.GRanges
)



upperCamel.GRanges <-  # nolint
    function(object, strict = FALSE) {
        colnames(mcols(object)) <- upperCamel(
            object = colnames(mcols(object)),
            strict = strict
        )
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("GRanges"),
    definition = upperCamel.GRanges
)



# GRangesList ==================================================================
camel.GRangesList <-  # nolint
    camel.GRanges

#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("GRangesList"),
    definition = camel.GRangesList
)



dotted.GRangesList <-  # nolint
    dotted.GRanges

#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("GRangesList"),
    definition = dotted.GRangesList
)



snake.GRangesList <-  # nolint
    snake.GRanges

#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("GRangesList"),
    definition = snake.GRangesList
)



upperCamel.GRangesList <-  # nolint
    upperCamel.GRanges

#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("GRangesList"),
    definition = upperCamel.GRangesList
)



# ANY ==========================================================================
camel.ANY <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = FALSE
    ) {
        if (!is.null(dimnames(object))) {
            camel.matrix(
                object = object,
                rownames = rownames,
                colnames = colnames,
                strict = strict
            )
        } else {
            object  # nocov
        }
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("ANY"),
    definition = camel.ANY
)



dotted.ANY <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        if (!is.null(dimnames(object))) {
            dotted.matrix(
                object = object,
                rownames = rownames,
                colnames = colnames
            )
        } else {
            object  # nocov
        }
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("ANY"),
    definition = dotted.ANY
)



snake.ANY <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        if (!is.null(dimnames(object))) {
            snake.matrix(
                object = object,
                rownames = rownames,
                colnames = colnames
            )
        } else {
            object  # nocov
        }
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("ANY"),
    definition = snake.ANY
)



upperCamel.ANY <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = FALSE
    ) {
        if (!is.null(dimnames(object))) {
            upperCamel.matrix(
                object = object,
                rownames = rownames,
                colnames = colnames,
                strict = strict
            )
        } else {
            object  # nocov
        }
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("ANY"),
    definition = upperCamel.ANY
)
