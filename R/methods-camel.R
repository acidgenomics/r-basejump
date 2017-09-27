# Constructors ====
.makeNamesCamel <- function(object, strict = TRUE) {
    object <- object %>%
        .makeNamesDotted(strict = strict) %>%
        gsub("\\.(\\w?)", "\\U\\1", ., perl = TRUE)
    # Coerce string starting with capitalized acronym
    if (str_detect(object, "^[A-Z0-9]+[a-z]")) {
        object <- gsub("^([A-Z0-9]+)([A-Z])", "\\L\\1\\U\\2", object, perl = TRUE)
    }
    # Fix capitalization at beginning
    if (str_detect(object, "^[A-Z0-9]+")) {
        object <- gsub("^([A-Z0-9]+)", "\\L\\1", object, perl = TRUE)
    }
    object
}



.setNamesCamel <- function(object, rownames = FALSE, strict = TRUE) {
    if (.checkNames(object)) {
        object <- setNames(
            object,
            .makeNamesCamel(names(object), strict = strict))
    }
    if (isTRUE(rownames) & .checkRownames(object)) {
        object <- set_rownames(
            object,
            .makeNamesCamel(rownames(object), strict = strict))
    }
    object
}



# Methods ====
#' @rdname makeNames
#' @export
setMethod("camel", "ANY", .setNamesCamel)



#' @rdname makeNames
#' @export
setMethod("camel", "character", function(object, strict = TRUE) {
    if (isTRUE(.checkNames(object))) {
        .setNamesCamel(object, rownames = FALSE, strict = strict)
    } else {
        .makeNamesCamel(object, strict = strict)
    }
})



#' @rdname makeNames
#' @export
setMethod("camel", "data.frame", .setNamesCamel)



#' @rdname makeNames
#' @export
setMethod("camel", "list", function(object, strict = TRUE) {
    .setNamesCamel(object, rownames = FALSE, strict = strict)
})



#' @rdname makeNames
#' @export
setMethod("camel", "matrix", .setNamesCamel)



#' @rdname makeNames
#' @export
setMethod("camel", "tbl_df", function(object, strict = TRUE) {
    .setNamesCamel(object, rownames = FALSE, strict = strict)
})
