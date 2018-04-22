#' `grep` String
#'
#' Generate a `grep` string for pattern matching against a delimited `character`
#' string returned by [base::toString()] or [base::paste()] using the `collapse`
#' argument.
#'
#' @family Atomic Vector Functions
#' @author Michael Steinbaugh
#'
#' @param string `character` string.
#' @param sep Separator. Defaults to comma.
#'
#' @return `character` string, containing a regular expression pattern.
#' @export
#'
#' @seealso
#' - [base::grep()].
#' - [base::toString()].
#'
#' @examples
#' grepString("gene")
grepString <- function(string, sep = ", ") {
    assert_is_a_string(string)
    sep <- sub(" ", "\\s", sep, fixed = TRUE)
    paste(
        # Unique
        paste0("^", string, "$"),
        # Beginning
        paste0("^", string, sep),
        # Middle
        paste0(sep, string, sep),
        # End
        paste0(sep, string, "$"),
        sep = "|"
    )
}
