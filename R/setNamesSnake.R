#' Set names of tibble as snake_case
#'
#' @importFrom magrittr set_names
#'
#' @param tibble 
#'
#' @return tibble
#' @export
setNamesSnake <- function(tibble) {
    tibble %>%
        magrittr::set_names(., snake(names(.)))
}
