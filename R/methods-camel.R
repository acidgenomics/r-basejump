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
#' camel(dataFrame, rownames = FALSE)
#' camel(dataFrame, rownames = TRUE)
#'
#' # Named list
#' list <- makeNames$list
#' print(list)
#' camel(list)
NULL



# Constructors =================================================================
.makeNamesCamel <- function(
    object,
    format = "lower",
    strict = FALSE) {
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



.makeNamesLowerCamel <- function(
    object,
    strict = FALSE) {
    .makeNamesCamel(
        object,
        format = "lower",
        strict = strict)
}



#' @importFrom magrittr set_rownames
.setNamesCamel <- function(
    object,
    format = "lower",
    rownames = FALSE,
    strict = FALSE) {
    if (.checkNames(object)) {
        object <- setNames(
            object,
            .makeNamesCamel(
                names(object),
                format = format,
                strict = strict))
    }
    if (isTRUE(rownames) & .checkRownames(object)) {
        object <- set_rownames(
            object,
            .makeNamesCamel(
                rownames(object),
                format = format,
                strict = strict))
    }
    object
}



.setNamesLowerCamel <- function(
    object,
    rownames = FALSE,
    strict = FALSE) {
    .setNamesCamel(
        object,
        format = "lower",
        rownames = rownames,
        strict = strict)
}



.setNamesLowerCamelNoRownames <- function(object, strict = FALSE) {
    .setNamesLowerCamel(
        object,
        rownames = FALSE,
        strict = strict)
}



.camelVector <- function(
    object,
    strict = FALSE) {
    if (isTRUE(.checkNames(object))) {
        names <- .makeNamesLowerCamel(names(object), strict = strict)
    } else {
        names <- NULL
    }
    object <- .makeNamesLowerCamel(object, strict = strict)
    names(object) <- names
    object
}



# Methods ======================================================================
#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("ANY"),
    .setNamesLowerCamel)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("character"),
    .camelVector)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("data.frame"),
    .setNamesLowerCamel)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("DataFrame"),
    .setNamesLowerCamel)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("factor"),
    .camelVector)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("list"),
    .setNamesLowerCamelNoRownames)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("matrix"),
    .setNamesLowerCamel)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("tbl_df"),
    .setNamesLowerCamelNoRownames)
