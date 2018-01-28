# Constructors =================================================================
.makeNamesUpperCamel <- function(object, strict = FALSE) {
    .makeNamesCamel(object, format = "upper", strict = strict)
}



.setNamesUpperCamel <- function(object, rownames = FALSE, strict = FALSE) {
    .setNamesCamel(
        object,
        format = "upper",
        rownames = rownames,
        strict = strict)
}



.setNamesUpperCamelNoRownames <- function(object, strict = FALSE) {
    .setNamesUpperCamel(object, rownames = FALSE, strict = strict)
}



.upperCamelVector <- function(object, strict = FALSE) {
    if (!is.null(names(object))) {
        names <- .makeNamesUpperCamel(names(object), strict = strict)
    } else {
        names <- NULL
    }
    object <- .makeNamesUpperCamel(object, strict = strict)
    names(object) <- names
    object
}



# Methods ======================================================================
#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("ANY"),
    .setNamesUpperCamel)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("character"),
    .upperCamelVector)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("data.frame"),
    .setNamesUpperCamel)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("DataFrame"),
    .setNamesUpperCamel)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("factor"),
    .upperCamelVector)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("list"),
    .setNamesUpperCamelNoRownames)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("matrix"),
    .setNamesUpperCamel)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("tbl_df"),
    .setNamesUpperCamelNoRownames)
