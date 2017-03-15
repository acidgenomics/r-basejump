#' Set names as snake_case
#'
#' @author Michael Steinbaugh
#' @keywords general
#'
#' @param data \code{data.frame}, \code{list}, or \code{tibble}
#'
#' @return data Data with reformatted snake_case names
#' @export
setNamesSnake <- function(data) {
    setNames(data, snake(colnames(data)))
}
