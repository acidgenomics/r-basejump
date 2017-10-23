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



# Constructors ====
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
        object <- setNames(
            object,
            .makeNamesSnake(names(object)))
    }
    if (isTRUE(rownames) &
        .checkRownames(object)) {
        object <- set_rownames(
            object,
            .makeNamesSnake(rownames(object)))
    }
    object
}



# Methods ====
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
    function(object) {
        if (isTRUE(.checkNames(object))) {
            .setNamesSnake(object, rownames = FALSE)
        } else {
            .makeNamesSnake(object)
        }
    })



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
    signature("list"),
    function(object) {
        .setNamesSnake(object, rownames = FALSE)
    })



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
    function(object) {
        .setNamesSnake(object, rownames = FALSE)
    })
