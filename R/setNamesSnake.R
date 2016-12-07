#' Set names as snake_case
#'
#' @importFrom magrittr set_names
#'
#' @param data \code{data.frame}, \code{list}, or \code{tibble}
#'
#' @return data Same data but with reformatted snake_case names
#' @export
setNamesSnake <- function(data) {
    data %>%
        magrittr::set_names(data, snake(names(.)))
}
