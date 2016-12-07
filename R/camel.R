#' camelCase
#'
#' @import magrittr
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
        # Convert non-alphanumeric characters to underscores:
        gsub("[^[:alnum:]]", "_", .) %>%
        # Multiple underscores to single:
        gsub("[_]+", "_", .) %>%
        # Remove leading or trailing underscores:
        gsub("(^_|_$)", "", .) %>%
        # Convert acronymes to Mixed Case:
        gsub("([A-Z]{1})([A-Z]+)", "\\1\\L\\2", ., perl = TRUE) %>%
        # Lowercase first letter:
        gsub("(^[A-Z]{1})", "\\L\\1", ., perl = TRUE) %>%
        # Convert snake_case to camelCase
        gsub("_(\\w?)", "\\U\\1", ., perl = TRUE)
}
