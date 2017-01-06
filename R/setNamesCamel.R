#' Set names as camelCase
#'
#' @export
#' @importFrom magrittr set_names
#' @keywords general
#' @param data \code{data.frame}, \code{list}, or \code{tibble}
#' @return data Same data but with reformatted camelCase names
setNamesCamel <- function(data) {
    data %>%
        magrittr::set_names(., camel(names(.)))
}
