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
    object %>%
        dotted() %>%
        tolower() %>%
        gsub("\\.", "_", .)
}



.snake.ANY <- function(
    object,
    rownames = FALSE,
    colnames = TRUE) {
    # Passthrough: rownames, colnames
    if (!is.null(dimnames(object))) {
        .snake.dim(object, rownames = rownames, colnames = colnames)
    } else if (!is.null(names(object))) {
        .snake.names(object)
    } else {
        object
    }
}



.snake.dim <- function(  # nolint
    object,
    rownames = FALSE,
    colnames = TRUE) {
    assert_has_dimnames(object)
    assert_is_a_bool(rownames)
    if (isTRUE(rownames) && has_rownames(object)) {
        rownames(object) <- .snake(rownames(object))
    }
    if (isTRUE(colnames) && has_colnames(object)) {
        colnames(object) <- .snake(colnames(object))
    }
    object
}



.snake.names <- function(object) {  # nolint
    assert_has_names(object)
    names(object) <- .snake(names(object))
    object
}


.snake.tibble <- function(object) {  # nolint
    .snake.dim(object, rownames = FALSE, colnames = TRUE)
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
    .snake.ANY)



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
    signature("List"),
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
    signature("SimpleList"),
    .snake.names)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("tbl_df"),
    .snake.tibble)
