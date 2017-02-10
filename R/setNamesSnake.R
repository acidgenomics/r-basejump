#' Set names as snake_case
#' @export
#' @keywords general
#' @param data \code{data.frame}, \code{list}, or \code{tibble}
#' @return data Data with reformatted snake_case names
setNamesSnake <- function(data) {
    setNames(data, snake(names(data)))
}
