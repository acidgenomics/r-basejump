#' Set names as snake_case
#'
#' @author Michael Steinbaugh
#'
#' @param data Data type that supports name assignments
#'
#' @return data Unmodified data with reformatted snake_case names
#' @export
#'
#' @examples
#' setNamesSnake(head(iris))
setNamesSnake <- function(data) {
    setNames(data, makeNamesSnake(colnames(data)))
}
