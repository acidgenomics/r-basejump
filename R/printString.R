#' Print String
#'
#' Capture [base::print()] output of an `atomic` vector. Useful for returning
#' informative messages inside a function.
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#'
#' @param x `atomic`.
#' @param max `scalar integer`. Maximum length of vector.
#'
#' @return `string`.
#' @export
#'
#' @seealso [base::cat()].
#'
#' @examples
#' printString(c("hello", "world"))
printString <- function(x, max = 100L) {
    assert_is_atomic(x)
    stopifnot(length(x) <= max)
    x <- capture.output(print(x))
    x <- paste(x, collapse = "\n")
    # Remove leading and trailing line breaks.
    x <- gsub("^[\n]+|[\n]+$", "", x)
    x
}
