#' Camel Case
#'
#' @name camel
#' @family Name Functions
#' @author Michael Steinbaugh
#'
#' @inherit dotted
#'
#' @param strict Enforce strict name sanitization. When `TRUE`, this does not
#'   allow the return of any capitalized acronyms. "RNA" will become "Rna", for
#'   example.
#'
#' @examples
#' load(system.file("extdata/mn.rda", package = "basejump"))
#'
#' # character ====
#' x <- mn$character
#' print(x)
#' camel(x)
#' upperCamel(x)
#'
#' x <- mn$namedCharacter
#' camel(x)
#' upperCamel(x)
#'
#' # factor ====
#' x <- mn$factor
#' print(x)
#' camel(x)
#' upperCamel(x)
#'
#' # data.frame ====
#' x <- mn$dataFrame
#' print(x)
#' camel(x, rownames = TRUE)
#' camel(x, rownames = FALSE)
#'
#' # list ====
#' x <- mn$list
#' print(x)
#' camel(x)
NULL



# Constructors =================================================================
.camel <- function(
    object,
    format = c("lower", "upper"),
    strict = FALSE
) {
    assert_is_character(object)
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



.camel.names <- function(  # nolint
    object,
    strict = FALSE
) {
    assert_has_names(object)
    names(object) <- camel(names(object), strict = strict)
    object
}



# Methods ======================================================================
#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("ANY"),
    function(object, strict = FALSE) {
        # Passthrough: rownames, colnames, strict
        if (!is.null(dimnames(object))) {
            .camel.dim(object, strict = strict)
        } else if (!is.null(names(object))) {
            .camel.names(object, strict = strict)
        } else {
            object
        }
    }
)



#' @rdname camel
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



#' @rdname camel
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



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("matrix"),
    .camel.dim
)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("data.frame"),
    getMethod("camel", "matrix")
)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("DataFrame"),
    getMethod("camel", "data.frame")
)



#' @rdname camel
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



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("list"),
    .camel.names
)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("List"),
    getMethod("camel", "list")
)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("SimpleList"),
    getMethod("camel", "list")
)
