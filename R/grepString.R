#' `grep` String
#'
#' Generate a `grep` string for pattern matching against a delimited `character`
#' string returned by [base::toString()] or [base::paste()] using the `collapse`
#' argument.
#'
#' @family Atomic Vector Functions
#' @author Michael Steinbaugh
#'
#' @param x `string`. Term to use for grep matching.
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
grepString <- function(x, sep = ", ") {
    assert_is_a_string(x)
    sep <- sub(" ", "\\s", sep, fixed = TRUE)
    paste(
        # Unique
        paste0("^", x, "$"),
        # Beginning
        paste0("^", x, sep),
        # Middle
        paste0(sep, x, sep),
        # End
        paste0(sep, x, "$"),
        sep = "|"
    )
}
