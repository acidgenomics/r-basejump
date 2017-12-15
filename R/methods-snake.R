#' Snake Case
#'
#' @rdname snake
#' @name snake
#'
#' @inherit dotted
#'
#' @examples
#' load(system.file(
#'     file.path("extdata", "makeNames.rda"),
#'     package = "basejump"))
#'
#' # Character vector
#' character <- makeNames$character
#' print(character)
#' snake(character)
#'
#' # Named character vector
#' namedCharacter <- makeNames$namedCharacter
#' print(namedCharacter)
#' snake(namedCharacter)
#'
#' # Factor
#' factor <- makeNames$factor
#' print(factor)
#' snake(factor)
#'
#' # data.frame
#' dataFrame <- makeNames$dataFrame
#' print(dataFrame)
#' snake(dataFrame, rownames = FALSE)
#' snake(dataFrame, rownames = TRUE)
#'
#' # Named list
#' list <- makeNames$list
#' print(list)
#' snake(list)
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
