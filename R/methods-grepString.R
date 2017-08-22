#' `grep` String Generator
#'
#' Generate a grep string for pattern matching against comma separated
#' [base::toString()] output.
#'
#' @rdname grepString
#' @name grepString
#'
#' @return Comma separated grep string.
#' @export
#'
#' @examples
#' grepString("gene")
NULL



# Methods ====
#' @rdname grepString
#' @export
setMethod("grepString", "character", function(object) {
    object %>%
        as.character %>%
        paste0(
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
