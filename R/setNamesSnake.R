#' Set names as snake_case
#'
#' @author Michael Steinbaugh
#'
#' @param data Data frame, list, tibble, etc.
#'
#' @return data Data with reformatted snake_case names
#' @export
setNamesSnake <- function(data) {
    setNames(data, snake(colnames(data)))
}
