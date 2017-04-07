#' @rdname setNames
#' @export
#' @examples
#' setNamesSnake(head(iris))
setNamesSnake <- function(data) {
    setNames(data, makeNamesSnake(colnames(data)))
}
