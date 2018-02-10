# Constructors =================================================================
.upperCamel.dim <- function(  # nolint
    object,
    rownames = FALSE,
    strict = FALSE) {
    .camel.dim(
        object,
        format = "upper",
        rownames = rownames,
        strict = strict)
}



.upperCamel.names <- function(object, strict = FALSE) {
    .upperCamel.dim(object, rownames = FALSE, strict = strict)
}



.upperCamel.vector <- function(object, strict = FALSE) {  # nolint
    if (!is.null(names(object))) {
        names <- .camel(names(object), format = "upper", strict = strict)
    } else {
        names <- NULL
    }
    object <- .camel(object, format = "upper", strict = strict)
    names(object) <- names
    object
}



# Methods ======================================================================
#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("ANY"),
    .upperCamel.dim)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("character"),
    .upperCamel.vector)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("data.frame"),
    .upperCamel.dim)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("DataFrame"),
    .upperCamel.dim)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("factor"),
    .upperCamel.vector)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("list"),
    .upperCamel.names)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("matrix"),
    .upperCamel.dim)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("tbl_df"),
    .upperCamel.names)
