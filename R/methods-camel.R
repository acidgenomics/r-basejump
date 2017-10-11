# Constructors ====
.makeNamesCamel <- function(object, strict = FALSE) {
    object %>%
        dotted(strict = strict) %>%
        # First word must be lowercase
        gsub(pattern = "^(\\w+)\\.",
             replacement = "\\L\\1.",
             x = .,
             perl = TRUE) %>%
        gsub(pattern = "\\.(\\w)",
             replacement = "\\U\\1",
             x = .,
             perl = TRUE) %>%
        # Coerce string starting with capitalized acronym
        gsub(pattern = "^([A-Z0-9]+)([A-Z])([a-z])",
             replacement = "\\L\\1\\U\\2\\L\\3",
             x = .,
             perl = TRUE) %>%
        # Fix capitalization at beginning
        gsub(pattern = "^([A-Z0-9]+)",
             replacement = "\\L\\1",
             x = .,
             perl = TRUE)
}



.setNamesCamel <- function(object, rownames = FALSE, strict = FALSE) {
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
setMethod("camel", "character", function(object, strict = FALSE) {
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
setMethod("camel", "list", function(object, strict = FALSE) {
    .setNamesCamel(object, rownames = FALSE, strict = strict)
})



#' @rdname makeNames
#' @export
setMethod("camel", "matrix", .setNamesCamel)



#' @rdname makeNames
#' @export
setMethod("camel", "tbl_df", function(object, strict = FALSE) {
    .setNamesCamel(object, rownames = FALSE, strict = strict)
})
