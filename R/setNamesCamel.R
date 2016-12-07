#' Set names as camelCase
#'
#' @importFrom magrittr set_names
#'
#' @param data \code{data.frame}, \code{list}, or \code{tibble}
#'
#' @return data Same data but with reformatted camelCase names
#' @export
setNamesCamel <- function(data) {
    data %>%
        magrittr::set_names(., camel(names(.)))
}
