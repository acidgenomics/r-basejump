#' Make Syntactically Valid Names
#'
#' For unnamed character vectors, this function will sanitize the underlying
#' values. Otherwise, it will set [names()] and/or [rownames()] on objects
#' supporting name assignments. They return the object without modification of
#' the underlying data.
#'
#' @note `dotted.case` support is provided for matching against base R
#'   parameters, but we strongly advise against using it for object and/or
#'   argument name assignments.
#'
#' @note [makeNames()] sanitizes names using underscore instead of dot.
#'
#' @name makeNames
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param object Character vector or an object for which [names()] assignment
#'   will be meaningful.
#' @param rownames Apply sanitization on row names. This is not recommended
#'   by default, since rownames commonly contain gene identifiers that should
#'   not be modified.
#' @param colnames Apply sanitization on column names. This is generally
#'   recommended by default.
#' @param strict Enforce strict name sanitization. When `TRUE`, this does not
#'   allow the return of any capitalized acronyms. "RNA" will become "Rna", for
#'   example.
#'
#' @return Object with syntatically valid names. For objects supporting
#'   [base::names()], the underlying data returns unchanged.
#'
#' @seealso [base::make.names()].
#'
#' @examples
#' load(system.file("extdata/mn.rda", package = "basejump"))
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
#' x <- mn$dataFrame
#' print(x)
#'
#' camel(x, rownames = FALSE)
#' camel(x, rownames = TRUE)
#'
#' dotted(x, rownames = FALSE)
#' dotted(x, rownames = TRUE)
#'
#' snake(x, rownames = FALSE)
#' snake(x, rownames = TRUE)
#'
#' upperCamel(x, rownames = FALSE)
#' upperCamel(x, rownames = TRUE)
#'
#' # list ====
#' x <- mn$list
#' print(x)
#' camel(x)
#' dotted(x)
#' snake(x)
#' upperCamel(x)
NULL



# Standard Functions ===========================================================
#' @rdname makeNames
#' @param names `atomic` to be coerced to syntactically valid names. Will be
#'   coerced to `character` if necessary.
#' @param unique `logical`; if `TRUE`, the resulting elements are unique.
#' @export
makeNames <- function(names, unique = FALSE) {
    assert_is_atomic(names)
    assert_is_a_bool(unique)
    names <- as.character(names)
    names <- make.names(names, unique = unique)
    names <- gsub("\\.", "_", names)
    names
}



# Constructors =================================================================
# atomic -----------------------------------------------------------------------
.camel <- function(
    object,
    format = c("lower", "upper"),
    strict = FALSE
) {
    assert_is_atomic(object)
    format <- match.arg(format)
    assert_is_a_bool(strict)

    object <- dotted(object)
    if (isTRUE(strict)) {
        object <- tolower(object)
    }

    # lowerCamelCase or UpperCamelCase
    if (format == "lower") {
        # lowerCamelCase
        # Coerce first word to lower
        object <- gsub("^(\\w+)\\b", "\\L\\1", object, perl = TRUE)
    } else if (format == "upper") {
        # UpperCamelCase
        # Capitalize the first letter
        object <- gsub("^([a-z])", replacement = "\\U\\1", object, perl = TRUE)
    }

    # Check for the presence of delimited numbers (e.g. 1,000,000)
    pattern <- "([0-9])\\.([0-9])"
    replacement <- "\\1x\\2"
    if (any(grepl(pattern, object))) {
        object <- object %>%
            # Escape number separators (useful for keeping decimals, etc.)
            gsub(pattern, replacement, .) %>%
            # Have to run twice here otherwise it will miss some matches
            gsub(pattern, replacement, .)
    }

    object %>%
        # First letter of second plus words must be capitalized
        gsub("\\.(\\w)", "\\U\\1", ., perl = TRUE)
}



# Dotted case is the internal method used by camel and snake
.dotted <- function(object) {
    assert_is_atomic(object)
    object %>%
        as.character() %>%
        make.names(unique = FALSE, allow_ = FALSE) %>%
        # Convert non-alphanumeric characters to dots
        gsub("[^[:alnum:]]", ".", .) %>%
        # Combine multiple dots
        gsub("[\\.]+", ".", .) %>%
        # Strip leading or trailing dots
        gsub("(^\\.|\\.$)", "", .) %>%
        # Coerce `"NA"` back to `NA` after `make.names()`
        fixNA() %>%
        .sanitizeAcronyms() %>%
        # Establish word boundaries for camelCase acronyms
        # (e.g. `worfdbHTMLRemap` -> `worfdb.HTML.remap`)
        # Acronym following a word
        gsub("([a-z])([A-Z])", "\\1.\\2", .) %>%
        # Word following an acronym
        gsub("([A-Z0-9])([A-Z])([a-z])", "\\1.\\2\\3", .)
}



.sanitizeAcronyms <- function(object) {
    assert_is_atomic(object)
    object %>%
        as.character() %>%
        # Ensure "id" is always "ID"
        gsub("\\b(id)\\b", "ID", ., ignore.case = TRUE) %>%
        # Sanitize mixed case scientific acronyms
        gsub("\\b(mRNA)\\b", "MRNA", .) %>%
        gsub("\\b(miRNA)\\b", "MIRNA", .) %>%
        gsub("\\b(ncRNA)\\b", "NCRNA", .) %>%
        gsub("\\b(piRNA)\\b", "PIRNA", .) %>%
        gsub("\\b(rRNA)\\b", "RRNA", .) %>%
        gsub("\\b(RNAi)\\b", "RNAI", .)
}



.snake <- function(object) {
    assert_is_atomic(object)
    object %>%
        dotted() %>%
        tolower() %>%
        gsub("\\.", "_", .)
}



.upperCamel <- function(object, strict = FALSE) {
    assert_is_atomic(object)
    .camel(object, format = "upper", strict = strict)
}



# dim --------------------------------------------------------------------------
.camel.dim <- function(  # nolint
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



.dotted.dim <- function(  # nolint
    object,
    rownames = FALSE,
    colnames = TRUE
) {
    assert_has_dimnames(object)
    assert_is_a_bool(rownames)
    if (isTRUE(rownames) && hasRownames(object)) {
        rownames(object) <- .dotted(rownames(object))
    }
    if (isTRUE(colnames) && has_colnames(object)) {
        colnames(object) <- .dotted(colnames(object))
    }
    object
}



.snake.dim <- function(  # nolint
    object,
    rownames = FALSE,
    colnames = TRUE
) {
    assert_has_dimnames(object)
    assert_is_a_bool(rownames)
    if (isTRUE(rownames) && hasRownames(object)) {
        rownames(object) <- .snake(rownames(object))
    }
    if (isTRUE(colnames) && has_colnames(object)) {
        colnames(object) <- .snake(colnames(object))
    }
    object
}



.upperCamel.dim <- function(  # nolint
    object,
    rownames = FALSE,
    colnames = TRUE,
    strict = FALSE
) {
    assert_has_dimnames(object)
    assert_is_a_bool(rownames)
    if (isTRUE(rownames) && hasRownames(object)) {
        rownames(object) <- .upperCamel(rownames(object), strict = strict)
    }
    if (isTRUE(colnames) && has_colnames(object)) {
        colnames(object) <- .upperCamel(colnames(object), strict = strict)
    }
    object
}



# names ------------------------------------------------------------------------
.camel.names <- function(object, strict = FALSE) {  # nolint
    assert_has_names(object)
    names(object) <- camel(names(object), strict = strict)
    object
}



.dotted.names <- function(object) {  # nolint
    assert_has_names(object)
    names(object) <- .dotted(names(object))
    object
}



.snake.names <- function(object) {  # nolint
    assert_has_names(object)
    names(object) <- .snake(names(object))
    object
}



.upperCamel.names <- function(object, strict = FALSE) {  # nolint
    assert_has_names(object)
    names(object) <- .upperCamel(names(object), strict = strict)
    object
}



# Methods ======================================================================
# ANY --------------------------------------------------------------------------
#' @rdname makeNames
#' @export
setMethod(
    "camel",
    signature("ANY"),
    function(object, strict = FALSE) {
        if (!is.null(dimnames(object))) {
            .camel.dim(object, strict = strict)
        } else if (!is.null(names(object))) {
            .camel.names(object, strict = strict)
        } else {
            object
        }
    }
)



#' @rdname makeNames
#' @export
setMethod(
    "dotted",
    signature("ANY"),
    function(object) {
        # Passthrough: rownames, colnames
        if (!is.null(dimnames(object))) {
            .dotted.dim(object)
        } else if (!is.null(names(object))) {
            .dotted.names(object)
        } else {
            object
        }
    }
)



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("ANY"),
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        # Passthrough: rownames, colnames
        if (!is.null(dimnames(object))) {
            .snake.dim(object, rownames = rownames, colnames = colnames)
        } else if (!is.null(names(object))) {
            .snake.names(object)
        } else {
            object
        }
    }
)



#' @rdname makeNames
#' @export
setMethod(
    "upperCamel",
    signature("ANY"),
    function(object, strict = FALSE) {
        if (!is.null(dimnames(object))) {
            .upperCamel.dim(object, strict = strict)
        } else if (!is.null(names(object))) {
            .upperCamel.names(object, strict = strict)
        } else {
            object
        }
    }
)



# character --------------------------------------------------------------------
#' @rdname makeNames
#' @export
setMethod(
    "camel",
    signature("character"),
    function(object, strict = FALSE) {
        if (!is.null(names(object))) {
            names <- .camel(names(object), strict = strict)
        } else {
            names <- NULL
        }
        object <- .camel(object, strict = strict)
        names(object) <- names
        object
    }
)



#' @rdname makeNames
#' @export
setMethod(
    "dotted",
    signature("character"),
    function(object) {
        if (!is.null(names(object))) {
            names <- .dotted(names(object))
        } else {
            names <- NULL
        }
        object <- .dotted(object)
        names(object) <- names
        object
    }
)



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("character"),
    function(object) {
        if (!is.null(names(object))) {
            names <- .snake(names(object))
        } else {
            names <- NULL
        }
        object <- .snake(object)
        names(object) <- names
        object
    }
)



#' @rdname makeNames
#' @export
setMethod(
    "upperCamel",
    signature("character"),
    function(object, strict = FALSE) {
        if (!is.null(names(object))) {
            names <- .upperCamel(names(object), strict = strict)
        } else {
            names <- NULL
        }
        object <- .upperCamel(object, strict = strict)
        names(object) <- names
        object
    }
)



# factor -----------------------------------------------------------------------
#' @rdname makeNames
#' @export
setMethod(
    "camel",
    signature("factor"),
    function(object, strict = FALSE) {
        object %>%
            as.character() %>%
            camel(strict = strict) %>%
            as.factor()
    }
)



#' @rdname makeNames
#' @export
setMethod(
    "dotted",
    signature("factor"),
    function(object) {
        object %>%
            as.character() %>%
            dotted() %>%
            as.factor()
    }
)



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("factor"),
    function(object) {
        object %>%
            as.character() %>%
            snake() %>%
            as.factor()
    }
)



#' @rdname makeNames
#' @export
setMethod(
    "upperCamel",
    signature("factor"),
    function(object, strict = FALSE) {
        object %>%
            as.character() %>%
            upperCamel(strict = strict) %>%
            as.factor()
    }
)



# matrix -----------------------------------------------------------------------
#' @rdname makeNames
#' @export
setMethod(
    "camel",
    signature("matrix"),
    .camel.dim
)



#' @rdname makeNames
#' @export
setMethod(
    "dotted",
    signature("matrix"),
    .dotted.dim
)



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("matrix"),
    .snake.dim
)



#' @rdname makeNames
#' @export
setMethod(
    "upperCamel",
    signature("matrix"),
    .upperCamel.dim
)



# data.frame -------------------------------------------------------------------
#' @rdname makeNames
#' @export
setMethod(
    "camel",
    signature("data.frame"),
    getMethod("camel", "matrix")
)



#' @rdname makeNames
#' @export
setMethod(
    "dotted",
    signature("data.frame"),
    getMethod("dotted", "matrix")
)



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("data.frame"),
    getMethod("snake", "matrix")
)



#' @rdname makeNames
#' @export
setMethod(
    "upperCamel",
    signature("data.frame"),
    getMethod("upperCamel", "matrix")
)



# DataFrame --------------------------------------------------------------------
#' @rdname makeNames
#' @export
setMethod(
    "camel",
    signature("DataFrame"),
    getMethod("camel", "data.frame")
)



#' @rdname makeNames
#' @export
setMethod(
    "dotted",
    signature("DataFrame"),
    getMethod("dotted", "data.frame")
)



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("DataFrame"),
    getMethod("snake", "data.frame")
)



#' @rdname makeNames
#' @export
setMethod(
    "upperCamel",
    signature("DataFrame"),
    getMethod("upperCamel", "data.frame")
)



# GRanges ----------------------------------------------------------------------
#' @rdname makeNames
#' @export
setMethod(
    "camel",
    signature("GRanges"),
    function(object, strict = FALSE) {
        colnames <- colnames(mcols(object))
        colnames <- camel(colnames, strict = strict)
        colnames(mcols(object)) <- colnames
        object
    }
)



#' @rdname makeNames
#' @export
setMethod(
    "dotted",
    signature("GRanges"),
    function(object) {
        colnames <- colnames(mcols(object))
        colnames <- dotted(colnames)
        colnames(mcols(object)) <- colnames
        object
    }
)



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("GRanges"),
    function(object) {
        colnames <- colnames(mcols(object))
        colnames <- snake(colnames)
        colnames(mcols(object)) <- colnames
        object
    }
)



#' @rdname makeNames
#' @export
setMethod(
    "upperCamel",
    signature("GRanges"),
    function(object, strict = FALSE) {
        colnames <- colnames(mcols(object))
        colnames <- upperCamel(colnames, strict = strict)
        colnames(mcols(object)) <- colnames
        object
    }
)



# list -------------------------------------------------------------------------
#' @rdname makeNames
#' @export
setMethod(
    "camel",
    signature("list"),
    .camel.names
)



#' @rdname makeNames
#' @export
setMethod(
    "dotted",
    signature("list"),
    .dotted.names
)



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("list"),
    .snake.names
)



#' @rdname makeNames
#' @export
setMethod(
    "upperCamel",
    signature("list"),
    .upperCamel.names
)



# List -------------------------------------------------------------------------
#' @rdname makeNames
#' @export
setMethod(
    "camel",
    signature("List"),
    getMethod("camel", "list")
)



#' @rdname makeNames
#' @export
setMethod(
    "dotted",
    signature("List"),
    getMethod("dotted", "list")
)



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("List"),
    getMethod("snake", "list")
)



#' @rdname makeNames
#' @export
setMethod(
    "upperCamel",
    signature("List"),
    getMethod("upperCamel", "list")
)



# SimpleList -------------------------------------------------------------------
#' @rdname makeNames
#' @export
setMethod(
    "camel",
    signature("SimpleList"),
    getMethod("camel", "list")
)



#' @rdname makeNames
#' @export
setMethod(
    "dotted",
    signature("SimpleList"),
    getMethod("dotted", "list")
)



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("SimpleList"),
    getMethod("snake", "list")
)



#' @rdname makeNames
#' @export
setMethod(
    "upperCamel",
    signature("SimpleList"),
    getMethod("upperCamel", "list")
)
