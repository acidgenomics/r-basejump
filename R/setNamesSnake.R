#' Set names as snake_case
#' @export
#' @importFrom stats setNames
#' @keywords general
#' @param data \code{data.frame}, \code{list}, or \code{tibble}
#' @return data Data with reformatted snake_case names
setNamesSnake <- function(data) {
    data %>%
        stats::setNames(data, snake(names(.)))
}
