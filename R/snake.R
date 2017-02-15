#' snake_case
#' @export
#' @param string \code{string}
#' @return snake_case formatted \code{string}
#' @examples
#' snake("RNAi clone")
snake <- function(string) {
    string %>%
        specialWords %>%
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
