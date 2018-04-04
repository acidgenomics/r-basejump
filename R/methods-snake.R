#' Snake Case
#'
#' @name snake
#' @family Name Functions
#' @author Michael Steinbaugh
#'
#' @inherit dotted
#'
#' @examples
#' load(system.file("extdata/mn.rda", package = "basejump"))
#'
#' # character ====
#' character <- mn$character
#' print(character)
#' snake(character)
#'
#' namedCharacter <- mn$namedCharacter
#' print(namedCharacter)
#' snake(namedCharacter)
#'
#' # factor ====
#' factor <- mn$factor
#' print(factor)
#' snake(factor)
#'
#' # data.frame ====
#' dataFrame <- mn$dataFrame
#' print(dataFrame)
#' snake(dataFrame, rownames = TRUE)
#' snake(dataFrame, rownames = FALSE)
#'
#' # list ====
#' list <- mn$list
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



.snake.ANY <- function(  # nolint
    object,
    rownames = FALSE,
    colnames = TRUE
) {
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
    colnames = TRUE
) {
    assert_has_dimnames(object)
    assert_is_a_bool(rownames)
    if (isTRUE(rownames) && hasRownames(object)) {
        rownames(object) <- .snake(rownames(object))
    }
    if (isTRUE(colnames) && has_colnames(object)) {
        colnames(object) <- .snake(colnames(object))
    }
    object
}



.snake.factor <- function(object) {  # nolint
    object %>%
        .snake.vector() %>%
        factor()
}



.snake.mcols <- function(object) {  # nolint
    colnames <- colnames(mcols(object))
    colnames <- snake(colnames)
    colnames(mcols(object)) <- colnames
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
    .snake.ANY
)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("character"),
    .snake.vector
)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("data.frame"),
    .snake.dim
)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("DataFrame"),
    .snake.dim
)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("factor"),
    .snake.factor
)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("GRanges"),
    .snake.mcols
)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("list"),
    .snake.names
)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("List"),
    .snake.names
)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("matrix"),
    .snake.dim
)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("SimpleList"),
    .snake.names
)



#' @rdname snake
#' @export
setMethod(
    "snake",
    signature("tbl_df"),
    .snake.tibble
)
