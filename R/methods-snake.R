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
#' snake(dataFrame, rownames = TRUE)
#' snake(dataFrame, rownames = FALSE)
#'
#' # Named list
#' list <- makeNames$list
#' print(list)
#' snake(list)
NULL



# Constructors =================================================================
.snake <- function(object) {
    .checkCharacter(object)
    object %>%
        dotted() %>%
        tolower() %>%
        gsub("\\.", "_", .)
}



.snake.dim <- function(object, rownames = FALSE) {  # nolint
    if (!is.logical(rownames)) {
        abort("`rownames` must be logical")
    }
    if (!is.null(dimnames(object))) {
        # Colnames
        if (!is.null(colnames(object))) {
            colnames(object) <- .snake(colnames(object))
        }
        # Rownames
        if (isTRUE(rownames) & .checkRownames(object)) {
            rownames(object) <- .snake(rownames(object))
        }
    } else if (!is.null(names(object))) {
        names(object) <- .snake(names(object))
    }
    object
}



.snake.names <- function(object) {  # nolint
    .snake.dim(object, rownames = FALSE)
}



.snake.vector <- function(object) {  # nolint
    if (!is.null(names(object))) {
        names <- .snake(names(object))
    } else {
        names <- NULL
    }
    object <- .snake(object)
    names(object) <- names
    object
}



# Methods ======================================================================
#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("ANY"),
    .snake.dim)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("character"),
    .snake.vector)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("data.frame"),
    .snake.dim)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("DataFrame"),
    .snake.dim)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("factor"),
    .snake.vector)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("list"),
    .snake.names)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("matrix"),
    .snake.dim)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("tbl_df"),
    .snake.names)
