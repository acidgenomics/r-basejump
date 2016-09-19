#' snake_case
#'
#' @import dplyr
#'
#' @param string \code{string}.
#'
#' @return \code{string} with snake_case formatting.
#' @export
#'
#' @examples
#' snake("RNAi clone")
snake <- function(string) {
    string %>%
        # Remove parentheses:
        gsub("(\\(|\\))", "", .) %>%
        # Convert all periods, dashes, spaces to underscores:
        gsub("(\\.|-| )", "_", .) %>%
        # Convert acronymes to Mixed Case:
        gsub("([A-Z]{1})([A-Z]+)", "\\1\\L\\2", ., perl = TRUE) %>%
        # Convert camelCase to snake_case
        gsub("([a-z0-9])([A-Z])", "\\1_\\L\\2", ., perl = TRUE) %>%
        tolower
}
