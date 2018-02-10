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
.camel <- function(  # nolint
    object,
    format = "lower",
    strict = FALSE) {
    .checkCharacterOrFactor(object)
    validFormats <- c("lower", "upper")
    if (!format %in% validFormats) {
        abort(paste(
            "Valid formats:",
            toString(validFormats)
        ))
    }
    .checkStrict(strict)

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
    format = "lower",
    rownames = FALSE,
    strict = FALSE) {
    # Passthrough: format, strict
    assert_is_a_boolean(rownames) {
    if (!is.null(dimnames(object))) {
        # Colnames
        if (!is.null(colnames(object))) {
            colnames(object) <- .camel(
                colnames(object),
                format = format,
                strict = strict)
        }
        # Rownames
        if (isTRUE(rownames) & .checkRownames(object)) {
            rownames(object) <- .camel(
                rownames(object),
                format = format,
                strict = strict)
        }
    } else if (!is.null(names(object))) {
        names(object) <- .camel(
            names(object),
            format = format,
            strict = strict)
    }
    object
}



.lowerCamel.dim <- function(  # nolint
    object,
    rownames = FALSE,
    strict = FALSE) {
    # Passthrough: rownames, strict
    .camel.dim(
        object,
        format = "lower",
        rownames = rownames,
        strict = strict)
}



.lowerCamel.names <- function(object, strict = FALSE) {  # nolint
    # Passthrough: strict
    .lowerCamel.dim(object, rownames = FALSE, strict = strict)
}



.lowerCamel.vector <- function(object, strict = FALSE) {  # nolint
    # Passthrough: strict
    if (!is.null(names(object))) {
        names <- .camel(
            names(object),
            format = "lower",
            strict = strict)
    } else {
        names <- NULL
    }
    object <- .camel(
        object,
        format = "lower",
        strict = strict)
    names(object) <- names
    object
}



# Methods ======================================================================
#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("ANY"),
    .lowerCamel.dim)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("character"),
    .lowerCamel.vector)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("data.frame"),
    .lowerCamel.dim)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("DataFrame"),
    .lowerCamel.dim)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("factor"),
    .lowerCamel.vector)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("list"),
    .lowerCamel.names)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("matrix"),
    .lowerCamel.dim)



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("tbl_df"),
    .lowerCamel.names)
