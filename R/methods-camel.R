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
#' loadRemoteData("http://basejump.seq.cloud/makeNames.rda")
#'
#' # Character vector
#' vector <- makeNames$vector
#' print(vector)
#' camel(vector)
#' upperCamel(vector)
#'
#' # Named character vector
#' namedVector <- makeNames$namedVector
#' camel(namedVector)
#' upperCamel(namedVector)
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
    if (format == "lower") {
        # lowerCamelCase
        # Coerce first word to lower
        object <- gsub(
            object,
            pattern = "^(\\w+)\\b",
            replacement = "\\L\\1",
            perl = TRUE)

    } else if (format == "upper") {
        # UpperCamelCase
        # Capitalize the first letter
        object <- gsub(
            object,
            pattern = "^([a-z])",
            replacement = "\\U\\1",
            perl = TRUE)
    }
    object %>%
        # First letter of second plus words must be capitalized
        gsub(x = .,
             pattern = "\\.(\\w)",
             replacement = "\\U\\1",
             perl = TRUE)
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
