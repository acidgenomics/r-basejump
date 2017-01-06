#' snake_case
#'
#' @import magrittr
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
        # Convert non-alphanumeric characters to underscores:
        gsub("[^[:alnum:]]", "_", .) %>%
        # Multiple underscores to single:
        gsub("[_]+", "_", .) %>%
        # Remove leading or trailing underscores:
        gsub("(^_|_$)", "", .) %>%
        # Convert acronymes to Mixed Case:
        gsub("([A-Z]{1})([A-Z]+)", "\\1\\L\\2", ., perl = TRUE) %>%
        # Convert camelCase to snake_case
        gsub("([a-z0-9])([A-Z])", "\\1_\\L\\2", ., perl = TRUE) %>%
        tolower
}
