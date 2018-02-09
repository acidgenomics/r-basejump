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
#'     package = "basejump"
#' ))
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
    .checkCharacter(object)
    object %>%
        dotted() %>%
        tolower() %>%
        gsub("\\.", "_", .)
}



.setNamesSnake <- function(object, rownames = FALSE) {
    if (!is.logical(rownames)) {
        abort("`rownames` must be logical")
    }
    if (!is.null(dimnames(object))) {
        # Colnames
        if (!is.null(colnames(object))) {
            colnames(object) <- .makeNamesSnake(colnames(object))
        }
        # Rownames
        if (isTRUE(rownames) & .checkRownames(object)) {
            rownames(object) <- .makeNamesSnake(rownames(object))
        }
    } else if (!is.null(names(object))) {
        names(object) <- .makeNamesSnake(names(object))
    }
    object
}



.setNamesSnakeNoRownames <- function(object) {
    .setNamesSnake(object, rownames = FALSE)
}



.snakeVector <- function(object) {
    if (!is.null(names(object))) {
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
