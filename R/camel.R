#' camelCase
#'
#' @import dplyr
#'
#' @param string \code{string}.
#'
#' @return \code{string} with camelCase formatting.
#' @export
#'
#' @examples
#' camel("RNAi clone")
#'
camel <- function(string) {
    string %>%
        # Remove parentheses:
        gsub("(\\(|\\))", "", .) %>%
        # Convert all periods, dashes, spaces to underscores:
        gsub("(\\.|-| )", "_", .) %>%
        # Convert acronymes to Mixed Case:
        gsub("([A-Z]{1})([A-Z]+)", "\\1\\L\\2", ., perl = TRUE) %>%
        # Lowercase first letter:
        gsub("(^[A-Z]{1})", "\\L\\1", ., perl = TRUE) %>%
        # Convert snake_case to camelCase
        gsub("_(\\w?)", "\\U\\1", ., perl = TRUE)
}
