#' snake_case
#'
#' @author Michael Steinbaugh
#' @keywords general
#'
#' @param string \code{string}
#'
#' @return snake_case formatted \code{string}
#' @export
#'
#' @examples
#' snake("RNAi clone")
snake <- function(string) {
    string %>%
        sanitize %>%
        # RNA types
        gsub("(m|nc|r)RNA", "\\1rna", .) %>%
        # Acronyms
        gsub("([A-Z])([A-Z]+)", "\\1\\L\\2", ., perl = TRUE) %>%
        # Capital first letter
        gsub("(^[A-Z]{1})", "\\L\\1", ., perl = TRUE) %>%
        # camelCase
        gsub("([a-z0-9])([A-Z])", "\\1_\\L\\2", ., perl = TRUE) %>%
        tolower
}
