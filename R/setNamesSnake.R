#' Set names as snake_case
#'
#' @export
#' @param data \code{data.frame}, \code{list}, or \code{tibble}
#' @return data Same data but with reformatted snake_case names
setNamesSnake <- function(data) {
    data %>%
        setNames(snake(names(.)))
}
