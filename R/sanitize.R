#' Sanitize
#'
#' @author Michael Steinbaugh
#' @keywords text
#'
#' @param string \code{string}
#'
#' @return Sanitized string
#' @export
sanitize <- function(string) {
    string %>%
        # Non-alphanumeric characters
        gsub("[^[:alnum:]]", "_", .) %>%
        # Multiple underscores
        gsub("[_]+", "_", .) %>%
        # Leading or trailing underscores
        gsub("(^_|_$)", "", .) %>%
        # Syntactically valid names
        make.names
}
