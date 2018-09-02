#' Generate a `grep` String
#'
#' A `grep` string is useful for pattern matching against a delimited
#' `character` string returned by [base::toString()] or [base::paste()] using
#' the `collapse` argument.
#'
#' @family Atomic Vector Functions
#' @author Michael Steinbaugh
#'
#' @param object `string`. Term to use for grep matching.
#' @param sep `string`. Separator. Defaults to comma.
#'
#' @return `string`, containing a regular expression pattern.
#' @export
#'
#' @seealso
#' - [base::grep()].
#' - [base::toString()].
#'
#' @examples
#' grepString("gene")
grepString <- function(object, sep = ", ") {
    assert_is_a_string(object)
    sep <- sub(" ", "\\s", sep, fixed = TRUE)
    paste(
        # Unique
        paste0("^", object, "$"),
        # Beginning
        paste0("^", object, sep),
        # Middle
        paste0(sep, object, sep),
        # End
        paste0(sep, object, "$"),
        sep = "|"
    )
}
