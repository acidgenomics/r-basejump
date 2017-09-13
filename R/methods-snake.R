# Constructors ====
.makeNamesSnake <- function(object) {
    object %>%
        .makeNamesDotted %>%
        tolower %>%
        str_replace_all("\\.", "_")
}



.setNamesSnake <- function(object, rownames = FALSE) {
    if (.checkNames(object)) {
        object <- setNames(object, .makeNamesSnake(names(object)))
    }
    if (isTRUE(rownames) & .checkRownames(object)) {
        object <- set_rownames(object, .makeNamesSnake(rownames(object)))
    }
    object
}



# Methods ====
#' @rdname makeNames
#' @export
setMethod("snake", "ANY", .setNamesSnake)



#' @rdname makeNames
#' @export
setMethod("snake", "character", function(object) {
    if (isTRUE(.checkNames(object))) {
        .setNamesSnake(object, rownames = FALSE)
    } else {
        .makeNamesSnake(object)
    }
})



#' @rdname makeNames
#' @export
setMethod("snake", "data.frame", .setNamesSnake)



#' @rdname makeNames
#' @export
setMethod("snake", "list", function(object) {
    .setNamesSnake(object, rownames = FALSE)
})



#' @rdname makeNames
#' @export
setMethod("snake", "matrix", .setNamesSnake)



#' @rdname makeNames
#' @export
setMethod("snake", "tbl_df", function(object) {
    .setNamesSnake(object, rownames = FALSE)
})
