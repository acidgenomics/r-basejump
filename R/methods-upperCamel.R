# Methods ====
#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("ANY"),
    function(object, strict = FALSE) {
        .setNamesCamel(
            object,
            format = "upper",
            strict = strict)
    }
)



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("character"),
    function(
        object,
        strict = FALSE) {
        if (isTRUE(.checkNames(object))) {
            .setNamesCamel(
                object,
                format = "upper",
                rownames = FALSE,
                strict = strict)
        } else {
            .makeNamesCamel(
                object,
                format = "upper",
                strict = strict)
        }
    })



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("data.frame"),
    function(
        object,
        rownames = FALSE,
        strict = FALSE) {
        .setNamesCamel(
            object,
            format = "upper",
            rownames = rownames,
            strict = strict)
    })



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("list"),
    function(object, strict = FALSE) {
        .setNamesCamel(
            object,
            format = "upper",
            rownames = FALSE,
            strict = strict)
    })



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("matrix"),
    function(
        object,
        rownames = FALSE,
        strict = FALSE) {
        .setNamesCamel(
            object,
            format = "upper",
            rownames = rownames,
            strict = strict)
    })



#' @rdname camel
#' @export
setMethod(
    "upperCamel",
    signature("tbl_df"),
    function(object, strict = FALSE) {
        .setNamesCamel(
            object,
            format = "upper",
            rownames = FALSE,
            strict = strict)
    })
