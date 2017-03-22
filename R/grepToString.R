#' Grep pattern for toString
#'
#' @author Michael Steinbaugh
#'
#' @keywords internal
#'
#' @param identifier Identifier
#'
#' @return Comma separated string for matching against \code{toString} output
#' @export
grepToString <- function(identifier) {
    identifier %>%
        paste0(
            # Unique:
            "^", ., "$",
            "|",
            # Beginning of list:
            "^", ., ",",
            "|",
            # Middle of list:
            "\\s", ., ",",
            "|",
            # End of list:
            "\\s", ., "$")
}
