#' Make Syntactically Valid Names
#'
#' For `atomic` vectors, these functions will sanitize the values. Otherwise,
#' they will set [names()], [rownames()], and/or [colnames()] without
#' modification of the values.
#'
#' @note
#' [makeNames()] sanitizes names using underscores instead of dots, the
#' convention used by [base::make.names()].
#'
#' `dotted.case` support is provided for matching against base R parameters.
#' However, it is recommended to avoid using it for variable assignments into an
#' `environment`, as that can introduce conflicts with base functions.
#'
#' @name makeNames
#' @family Sanitization Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param object Character vector or an object for which [names()] assignment
#'   will be meaningful.
#' @param rownames `boolean`. Apply sanitization on row names. This is not
#'   recommended by default, since rownames commonly contain gene identifiers
#'   that should not be modified.
#' @param colnames `boolean`. Apply sanitization on column names. This is
#'   generally recommended by default.
#' @param strict `boolean`. Enforce strict name sanitization. When `TRUE`, this
#'   does not allow the return of any capitalized acronyms. "RNA" will become
#'   "Rna", for example.
#'
#' @return Object with syntatically valid names. For objects supporting
#'   [names()], the underlying data returns unchanged.
#'
#' @seealso
#' - [make.names()].
#' - [janitor](https://cran.r-project.org/package=janitor) contains a number of
#'   useful functions that provide similar sanitization support, but isn't
#'   designed to work natively with [Bioconductor](https://bioconductor.org) and
#'   S4 object classes.
#' - [lettercase](https://cran.r-project.org/package=lettercase).
#' - [snakecase](https://tazinho.github.io/snakecase).
#'
#' @examples
#' loadRemoteData("http://basejump.seq.cloud/mn.rda")
#'
#' # character ====
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
#' # factor ====
#' x <- mn$factor
#' print(x)
#' camel(x)
#' dotted(x)
#' snake(x)
#' upperCamel(x)
#' makeNames(x)
#'
#' # data.frame ====
#' x <- datasets::USArrests
#' dimnames(x)
#' camel(x, rownames = TRUE, colnames = TRUE) %>% dimnames()
#' dotted(x, rownames = TRUE, colnames = TRUE) %>% dimnames()
#' snake(x, rownames = TRUE, colnames = TRUE) %>% dimnames()
#' upperCamel(x, rownames = TRUE, colnames = TRUE) %>% dimnames()
#'
#' # list ====
#' x <- mn$list
#' print(x)
#' camel(x) %>% names()
#' dotted(x) %>% names()
#' snake(x) %>% names()
#' upperCamel(x) %>% names()
NULL



# Constructors =================================================================
.sanitizeAcronyms <- function(object) {
    assert_is_atomic(object)
    object %>%
        as.character() %>%
        # Ensure "id" is always "ID".
        gsub("\\b(id)\\b", "ID", ., ignore.case = TRUE) %>%
        # Sanitize mixed case scientific acronyms.
        gsub("\\b(mRNA)\\b", "MRNA", .) %>%
        gsub("\\b(miRNA)\\b", "MIRNA", .) %>%
        gsub("\\b(ncRNA)\\b", "NCRNA", .) %>%
        gsub("\\b(piRNA)\\b", "PIRNA", .) %>%
        gsub("\\b(rRNA)\\b", "RRNA", .) %>%
        gsub("\\b(RNAi)\\b", "RNAI", .)
}



# atomic =======================================================================
#' @rdname makeNames
#' @inheritParams base::make.names
#' @export
makeNames <- function(names, unique = FALSE) {
    assert_is_atomic(names)
    assert_is_a_bool(unique)
    names <- as.character(names)
    names <- make.names(names, unique = unique)
    names <- gsub("\\.", "_", names)
    names
}



.camel.atomic <-  # nolint
    function(object, strict = FALSE) {
        if (has_names(object)) {
            names(object) <- .camel.character(names(object), strict = strict)
        }
        object
    }



.dotted.atomic <-  # nolint
    function(object) {
        if (has_names(object)) {
            names(object) <- .dotted.character(names(object))
        }
        object
    }



.snake.atomic <-  # nolint
    function(object) {
        if (has_names(object)) {
            names(object) <- .snake.character(names(object))
        }
        object
    }



.upperCamel.atomic <-  # nolint
    function(object, strict = FALSE) {
        if (has_names(object)) {
            names(object) <-
                .upperCamel.character(names(object), strict = strict)
        }
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("atomic"),
    definition = .camel.atomic
)



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("atomic"),
    definition = .dotted.atomic
)



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("atomic"),
    definition = .snake.atomic
)



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("atomic"),
    definition = .upperCamel.atomic
)



# character ====================================================================
.camel.character <-  # nolint
    function(
        object,
        format = c("lower", "upper"),
        strict = FALSE
    ) {
        object <- dotted(object)
        format <- match.arg(format)
        assert_is_a_bool(strict)

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

        # Check for the presence of delimited numbers (e.g. 100.00).
        pattern <- "([0-9])\\.([0-9])"
        if (isTRUE(strict)) {
            if (format == "lower") {
                replacement <- "x"
            } else if (format == "upper") {
                replacement <- "X"
            }
        } else {
            replacement <- "."
        }
        replacement <- paste0("\\1", replacement, "\\2")
        if (any(grepl(pattern, object))) {
            object <- object %>%
                # Escape number separators (useful for keeping decimals, etc.).
                gsub(pattern, replacement, .) %>%
                # Have to run twice here otherwise it will miss some matches.
                gsub(pattern, replacement, .)
        }

        # Remove dots in between numbers following a letter.
        object <- gsub("([[:alpha:]])\\.([[:digit:]])", "\\1\\2", object)

        # First letter of second word must be capitalized.
        object <- gsub("\\.([[:alpha:]])", "\\U\\1", object, perl = TRUE)

        object
    }



# Dotted case is the internal method used by camel and snake.
.dotted.character <-  # nolint
    function(object) {
        assert_is_atomic(object)
        object %>%
            as.character() %>%
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
            # Coerce `"NA"` back to `NA` after `make.names()`.
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



.snake.character <-  # nolint
    function(object) {
        assert_is_atomic(object)
        object %>%
            dotted() %>%
            tolower() %>%
            gsub("\\.", "_", .)
    }



.upperCamel.character <-  # nolint
    function(object, strict = FALSE) {
        .camel.character(object, format = "upper", strict = strict)
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("character"),
    definition = function(object, strict = FALSE) {
        if (has_names(object)) {
            names <- .camel.character(names(object), strict = strict)
        } else {
            names <- NULL
        }
        object <- .camel.character(object, strict = strict)
        names(object) <- names
        object
    }
)



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("character"),
    definition = function(object) {
        if (has_names(object)) {
            names <- .dotted.character(names(object))
        } else {
            names <- NULL
        }
        object <- .dotted.character(object)
        names(object) <- names
        object
    }
)



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("character"),
    definition = function(object) {
        if (has_names(object)) {
            names <- .snake.character(names(object))
        } else {
            names <- NULL
        }
        object <- .snake.character(object)
        names(object) <- names
        object
    }
)



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("character"),
    definition = function(object, strict = FALSE) {
        if (has_names(object)) {
            names <- .upperCamel.character(names(object), strict = strict)
        } else {
            names <- NULL
        }
        object <- .upperCamel.character(object, strict = strict)
        names(object) <- names
        object
    }
)



# factor =======================================================================
.camel.factor <-  # nolint
    function(object, strict = FALSE) {
        names <- names(object)
        object <- object %>%
            as.character() %>%
            camel(strict = strict) %>%
            as.factor()
        names(object) <- camel(names, strict = strict)
        object
    }



.dotted.factor <-  # nolint
    function(object) {
        names <- names(object)
        object <- object %>%
            as.character() %>%
            dotted() %>%
            as.factor()
        names(object) <- dotted(names)
        object
    }



.snake.factor <-  # nolint
    function(object) {
        names <- names(object)
        object <- object %>%
            as.character() %>%
            snake() %>%
            as.factor()
        names(object) <- snake(names)
        object
    }



.upperCamel.factor <-  # nolint
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
    f = "camel",
    signature = signature("factor"),
    definition = .camel.factor
)



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("factor"),
    definition = .dotted.factor
)



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("factor"),
    definition = .snake.factor
)



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("factor"),
    definition = .upperCamel.factor
)



# matrix =======================================================================
.camel.matrix <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = FALSE
    ) {
        assert_has_dimnames(object)
        assert_is_a_bool(rownames)
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- camel(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && has_colnames(object)) {
            assert_has_colnames(object)
            colnames(object) <- camel(colnames(object), strict = strict)
        }
        object
    }



.dotted.matrix <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        assert_has_dimnames(object)
        assert_is_a_bool(rownames)
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- .dotted.character(rownames(object))
        }
        if (isTRUE(colnames) && has_colnames(object)) {
            colnames(object) <- .dotted.character(colnames(object))
        }
        object
    }



.snake.matrix <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        assert_has_dimnames(object)
        assert_is_a_bool(rownames)
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- .snake.character(rownames(object))
        }
        if (isTRUE(colnames) && has_colnames(object)) {
            colnames(object) <- .snake.character(colnames(object))
        }
        object
    }



.upperCamel.matrix <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = FALSE
    ) {
        assert_has_dimnames(object)
        assert_is_a_bool(rownames)
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <-
                .upperCamel.character(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && has_colnames(object)) {
            colnames(object) <-
                .upperCamel.character(colnames(object), strict = strict)
        }
        object
    }



#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("matrix"),
    definition = .camel.matrix
)



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("matrix"),
    definition = .dotted.matrix
)



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("matrix"),
    definition = .snake.matrix
)



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("matrix"),
    definition = .upperCamel.matrix
)



# data.frame ===================================================================
#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("data.frame"),
    definition = getMethod("camel", "matrix")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("data.frame"),
    definition = getMethod("dotted", "matrix")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("data.frame"),
    definition = getMethod("snake", "matrix")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("data.frame"),
    definition = getMethod("upperCamel", "matrix")
)



# DataFrame ====================================================================
#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("DataFrame"),
    definition = getMethod("camel", "data.frame")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("DataFrame"),
    definition = getMethod("dotted", "data.frame")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("DataFrame"),
    definition = getMethod("snake", "data.frame")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("DataFrame"),
    definition = getMethod("upperCamel", "data.frame")
)



# GRanges ======================================================================
.camel.GRanges <-  # nolint
    function(object, strict = FALSE) {
        colnames(mcols(object)) <- camel(
            object = colnames(mcols(object)),
            strict = strict
        )
        object
    }



.dotted.GRanges <-  # nolint
    function(object) {
        colnames(mcols(object)) <- dotted(
            object = colnames(mcols(object))
        )
        object
    }



.snake.GRanges <-  # nolint
    function(object) {
        colnames(mcols(object)) <- snake(
            object = colnames(mcols(object))
        )
        object
    }



.upperCamel.GRanges <-  # nolint
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
    f = "camel",
    signature = signature("GRanges"),
    definition = .camel.GRanges
)



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("GRanges"),
    definition = .dotted.GRanges
)



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("GRanges"),
    definition = .snake.GRanges
)



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("GRanges"),
    definition = .upperCamel.GRanges
)



# CompressedGRangesList ========================================================
#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("CompressedGRangesList"),
    definition = getMethod("camel", "GRanges")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("CompressedGRangesList"),
    definition = getMethod("dotted", "GRanges")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("CompressedGRangesList"),
    definition = getMethod("snake", "GRanges")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("CompressedGRangesList"),
    definition = getMethod("upperCamel", "GRanges")
)



# list =========================================================================
#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("list"),
    definition = getMethod("camel", "atomic")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("list"),
    definition = getMethod("dotted", "atomic")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("list"),
    definition = getMethod("snake", "atomic")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("list"),
    definition = getMethod("upperCamel", "atomic")
)



# List =========================================================================
#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("List"),
    definition = getMethod("camel", "list")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("List"),
    definition = getMethod("dotted", "list")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("List"),
    definition = getMethod("snake", "list")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("List"),
    definition = getMethod("upperCamel", "list")
)



# SimpleList ===================================================================
#' @rdname makeNames
#' @export
setMethod(
    f = "camel",
    signature = signature("SimpleList"),
    definition = getMethod("camel", "list")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("SimpleList"),
    definition = getMethod("dotted", "list")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("SimpleList"),
    definition = getMethod("snake", "list")
)



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("SimpleList"),
    definition = getMethod("upperCamel", "list")
)



# ANY ==========================================================================
.camel.ANY <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = FALSE
    ) {
        if (!is.null(dimnames(object))) {
            .camel.matrix(
                object = object,
                rownames = rownames,
                colnames = colnames,
                strict = strict
            )
        } else {
            object  # nocov
        }
    }



.dotted.ANY <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        if (!is.null(dimnames(object))) {
            .dotted.matrix(
                object = object,
                rownames = rownames,
                colnames = colnames
            )
        } else {
            object  # nocov
        }
    }



.snake.ANY <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        if (!is.null(dimnames(object))) {
            .snake.matrix(
                object = object,
                rownames = rownames,
                colnames = colnames
            )
        } else {
            object  # nocov
        }
    }



.upperCamel.ANY <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = FALSE
    ) {
        if (!is.null(dimnames(object))) {
            .upperCamel.matrix(
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
    definition = .camel.ANY
)



#' @rdname makeNames
#' @export
setMethod(
    f = "dotted",
    signature = signature("ANY"),
    definition = .dotted.ANY
)



#' @rdname makeNames
#' @export
setMethod(
    f = "snake",
    signature = signature("ANY"),
    definition = .snake.ANY
)



#' @rdname makeNames
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("ANY"),
    definition = .upperCamel.ANY
)
