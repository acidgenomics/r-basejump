#' Set names of tibble as camelCase
#'
#' @importFrom magrittr set_names
#'
#' @param tibble 
#'
#' @return tibble
#' @export
setNamesCamel <- function(tibble) {
    tibble %>%
        magrittr::set_names(., camel(names(.)))
}
