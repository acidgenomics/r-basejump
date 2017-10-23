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
#' print(makeNames$vec)
#' camel(makeNames$vec)
#' upperCamel(makeNames$vec)
#'
#' # Named character vector
#' camel(makeNames$namedVec)
#' upperCamel(makeNames$vec)
#'
#' # data.frame
#' camel(makeNames$df)
#' camel(makeNames$df, rownames = TRUE)
#'
#' # Named list
#' camel(makeNames$lst)
NULL



# Constructors ====
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



# Methods ====
#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("ANY"),
    function(object, strict = FALSE) {
        .setNamesCamel(
            object,
            format = "lower",
            strict = strict)
    })



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("character"),
    function(
        object,
        strict = FALSE) {
        if (isTRUE(.checkNames(object))) {
            .setNamesCamel(
                object,
                format = "lower",
                rownames = FALSE,
                strict = strict)
        } else {
            .makeNamesCamel(
                object,
                format = "lower",
                strict = strict)
        }
    })



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("data.frame"),
    function(
        object,
        rownames = FALSE,
        strict = FALSE) {
        .setNamesCamel(
            object,
            format = "lower",
            rownames = rownames,
            strict = strict)
    })



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("list"),
    function(object, strict = FALSE) {
        .setNamesCamel(
            object,
            format = "lower",
            rownames = FALSE,
            strict = strict)
    })



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("matrix"),
    function(
        object,
        rownames = FALSE,
        strict = FALSE) {
        .setNamesCamel(
            object,
            format = "lower",
            rownames = rownames,
            strict = strict)
    })



#' @rdname camel
#' @export
setMethod(
    "camel",
    signature("tbl_df"),
    function(object, strict = FALSE) {
        .setNamesCamel(
            object,
            format = "lower",
            rownames = FALSE,
            strict = strict)
    })
