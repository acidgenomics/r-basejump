#' `grep` String Generator
#'
#' Generate a grep string for pattern matching against comma separated
#' [base::toString()] output.
#'
#' @rdname grepString
#'
#' @return Comma separated grep string.
#' @export
#'
#' @examples
#' grepString("gene")
setMethod("grepString", "character", function(object) {
    object %>%
        as.character %>%
        str_c(
            # Unique
            "^", ., "$",
            "|",
            # Beginning of list
            "^", ., ",",
            "|",
            # Middle of list
            "\\s", ., ",",
            "|",
            # End of list
            "\\s", ., "$")
})
