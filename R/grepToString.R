#' Grep against toString formatted data.
#'
#' Generates a grep pattern that will match an identifier in a comma separated
#' string.
#'
#' @param identifier Identifier.
#'
#' @return Comma separated string for matching against [toString()] return.
#' @export
#'
#' @examples
#' grepToString("gene")
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



#' @rdname grepToString
#' @export
grep_to_string <- grepToString
