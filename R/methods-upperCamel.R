# Constructors =================================================================
.upperCamel <- function(object, strict = FALSE) {
    .camel(object, format = "upper", strict = strict)
}



.upperCamel.ANY <- function(  # nolint
    object,
    rownames = FALSE,
    colnames = TRUE,
    strict = FALSE) {
    # Passthrough: rownames, colnames, strict
    if (!is.null(dimnames(object))) {
        .upperCamel.dim(
            object,
            rownames = rownames,
            colnames = colnames,
            strict = strict)
    } else if (!is.null(names(object))) {
        .upperCamel.names(object, strict = strict)
    } else {
        warn("Returning without upper camel case sanitization applied")
        object
    }
}



.upperCamel.dim <- function(  # nolint
    object,
    rownames = FALSE,
    colnames = TRUE,
    strict = FALSE) {
    # Passthrough: strict
    assert_has_dimnames(object)
    assert_is_a_bool(rownames)
    if (isTRUE(rownames) && has_rownames(object)) {
        rownames(object) <- .upperCamel(rownames(object), strict = strict)
    }
    if (isTRUE(colnames) && has_colnames(object)) {
        colnames(object) <- .upperCamel(colnames(object), strict = strict)
    }
    object
}



.upperCamel.names <- function(object, strict = FALSE) {
    # Passthrough: strict
    assert_has_names(object)
    names(object) <- .upperCamel(names(object), strict = strict)
    object
}



.upperCamel.tibble <- function(object, strict = FALSE) {  # nolint
    .upperCamel.dim(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = strict)
}



.upperCamel.vector <- function(object, strict = FALSE) {  # nolint
    if (!is.null(names(object))) {
        names <- .upperCamel(names(object), strict = strict)
    } else {
        names <- NULL
    }
    object <- .upperCamel(object, strict = strict)
    names(object) <- names
    object
}



# Methods ======================================================================
#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("ANY"),
    .upperCamel.ANY)



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



#' @rdname dotted
#' @export
setMethod(
    "upperCamel",
    signature("List"),
    .upperCamel.names)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("matrix"),
    .upperCamel.dim)



#' @rdname dotted
#' @export
setMethod(
    "upperCamel",
    signature("SimpleList"),
    .upperCamel.names)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("tbl_df"),
    .upperCamel.tibble)
