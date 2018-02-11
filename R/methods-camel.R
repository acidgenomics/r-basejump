#' Camel Case
#'
#' @rdname camel
#' @name camel
#'
#' @inherit dotted
#'
#' @param strict Enforce strict name sanitization. When `TRUE`, this does not
#'   allow the return of any capitalized acronyms. "RNA" will become "Rna", for
#'   example.
#'
#' @examples
#' load(system.file(
#'     file.path("extdata", "makeNames.rda"),
#'     package = "basejump"))
#'
#' # Character vector
#' character <- makeNames$character
#' print(character)
#' camel(character)
#' upperCamel(character)
#'
#' # Named character vector
#' namedCharacter <- makeNames$namedCharacter
#' camel(namedCharacter)
#' upperCamel(namedCharacter)
#'
#' # Factor
#' factor <- makeNames$factor
#' print(factor)
#' camel(factor)
#' upperCamel(factor)
#'
#' # data.frame
#' dataFrame <- makeNames$dataFrame
#' print(dataFrame)
#' camel(dataFrame, rownames = TRUE)
#' camel(dataFrame, rownames = FALSE)
#'
#' # Named list
#' list <- makeNames$list
#' print(list)
#' camel(list)
NULL



# Constructors =================================================================
.camel <- function(
    object,
    format = "lower",
    strict = FALSE) {
    object <- dotted(object)
    assert_is_a_string(format)
    assert_is_subset(format, c("lower", "upper"))
    assert_is_a_bool(strict)

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



.camel.ANY <- function(  # nolint
    object,
    rownames = FALSE,
    colnames = TRUE,
    strict = FALSE) {
    # Passthrough: rownames, colnames, strict
    if (!is.null(dimnames(object))) {
        .camel.dim(
            object,
            rownames = rownames,
            colnames = colnames,
            strict = strict)
    } else if (!is.null(names(object))) {
        .camel.names(object, strict = strict)
    } else {
        warn("Returning without lower camel case sanitization applied")
        object
    }
}



#' @importFrom assertive has_colnames has_rownames
.camel.dim <- function(  # nolint
    object,
    rownames = FALSE,
    colnames = TRUE,
    strict = FALSE) {
    # Passthrough: strict
    assert_has_dimnames(object)
    assert_is_a_bool(rownames)
    if (isTRUE(rownames) && has_rownames(object)) {
        rownames(object) <- .camel(rownames(object), strict = strict)
    }
    if (isTRUE(colnames) && has_colnames(object)) {
        assert_has_colnames(object)
        colnames(object) <- .camel(colnames(object), strict = strict)
    }
    object
}



.camel.names <- function(object, strict = FALSE) {  # nolint
    # Passthrough: strict
    assert_has_names(object)
    names(object) <- .camel(names(object), strict = strict)
}



.camel.tibble <- function(object, strict = FALSE) {  # nolint
    .camel.dim(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = strict)
}



.camel.vector <- function(object, strict = FALSE) {  # nolint
    # Passthrough: strict
    if (!is.null(names(object))) {
        names <- .camel(names(object), strict = strict)
    } else {
        names <- NULL
    }
    object <- .camel(object, strict = strict)
    names(object) <- names
    object
}



# Methods ======================================================================
#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("ANY"),
    .camel.ANY)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("character"),
    .camel.vector)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("data.frame"),
    .camel.dim)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("DataFrame"),
    .camel.dim)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("factor"),
    .camel.vector)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("list"),
    .camel.names)



#' @rdname dotted
#' @export
setMethod(
    "camel",
    signature("List"),
    .camel.names)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("matrix"),
    .camel.dim)



#' @rdname dotted
#' @export
setMethod(
    "camel",
    signature("SimpleList"),
    .camel.names)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("tbl_df"),
    .camel.tibble)
