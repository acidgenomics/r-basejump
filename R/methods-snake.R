# Constructors ====
.makeNamesSnake <- function(object) {
    object %>%
        # snake_case must always be lower case, so enforce strict here
        dotted(strict = TRUE) %>%
        tolower() %>%
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
setMethod(
    "snake",
    signature("ANY"),
    .setNamesSnake)



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("character"),
    function(object) {
        if (isTRUE(.checkNames(object))) {
            .setNamesSnake(object, rownames = FALSE)
        } else {
            .makeNamesSnake(object)
        }
    })



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("data.frame"),
    .setNamesSnake)



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("list"),
    function(object) {
        .setNamesSnake(object, rownames = FALSE)
    })



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("matrix"),
    .setNamesSnake)



#' @rdname makeNames
#' @export
setMethod(
    "snake",
    signature("tbl_df"),
    function(object) {
        .setNamesSnake(object, rownames = FALSE)
    })
