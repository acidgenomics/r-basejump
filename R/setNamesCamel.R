#' Set names as camelCase
#' @export
#' @importFrom stats setNames
#' @keywords general
#' @param data \code{data.frame}, \code{list}, or \code{tibble}
#' @return data Same data but with reformatted camelCase names
setNamesCamel <- function(data) {
    data %>%
        stats::setNames(., camel(names(.)))
}
