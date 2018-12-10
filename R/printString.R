#' Print String
#'
#' Capture `print()` output of an `atomic` vector. Useful for returning
#' informative messages inside a function.
#'
#' @export
#'
#' @param x `atomic`.
#' @param max `scalar integer`. Maximum length of vector. Works like
#'   `getOption("max.print")` without having to set globally.
#'
#' @return `string`.
#'
#' @seealso `cat()`.
#'
#' @examples
#' printString(c("hello", "world"))
printString <- function(x, max = 100L) {
    assert(is.atomic(x), isInt(max))
    x <- capture.output(print(x))
    # Limit the number of lines returned, like `max.print` option.
    x <- head(x, n = max)
    x <- paste(x, collapse = "\n")
    # Remove leading and trailing line breaks.
    x <- gsub("^[\n]+|[\n]+$", "", x)
    x
}
