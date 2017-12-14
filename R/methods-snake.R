#' Snake Case
#'
#' @rdname snake
#' @name snake
#'
#' @inherit dotted
#'
#' @examples
#' loadRemoteData("http://basejump.seq.cloud/makeNames.rda")
#'
#' # Character vector
#' snake(makeNames$vec)
#'
#' # Named character vector
#' snake(makeNames$namedVec)
#'
#' # data.frame
#' snake(makeNames$df, rownames = TRUE)
#'
#' # Named list
#' snake(makeNames$lst)
NULL



# Constructors =================================================================
.makeNamesSnake <- function(object) {
    object %>%
        dotted() %>%
        tolower() %>%
        gsub(x = .,
             pattern = "\\.",
             replacement = "_")
}



#' @importFrom magrittr set_rownames
#' @importFrom stats setNames
.setNamesSnake <- function(object, rownames = FALSE) {
    if (.checkNames(object)) {
        object <- setNames(object, .makeNamesSnake(names(object)))
    }
    if (isTRUE(rownames) &
        .checkRownames(object)) {
        rownames(object) <- .makeNamesSnake(rownames(object))
    }
    object
}



.setNamesSnakeNoRownames <- function(object) {
    .setNamesSnake(object, rownames = FALSE)
}



.snakeVector <- function(object) {
    if (isTRUE(.checkNames(object))) {
        names <- .makeNamesSnake(names(object))
    } else {
        names <- NULL
    }
    object <- .makeNamesSnake(object)
    names(object) <- names
    object
}



# Methods ======================================================================
#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("ANY"),
    .setNamesSnake)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("character"),
    .snakeVector)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("data.frame"),
    .setNamesSnake)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("DataFrame"),
    .setNamesSnake)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("factor"),
    .snakeVector)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("list"),
    .setNamesSnakeNoRownames)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("matrix"),
    .setNamesSnake)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("tbl_df"),
    .setNamesSnakeNoRownames)
