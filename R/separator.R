#' Separator Bar
#'
#' Maximum of 72 characters wide.
#'
#' @note Bioconductor HTML vignettes don't render correctly when printing > 76
#'   characters, even though the default width is set at 80.
#'
#' @family Developer Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @param sep `string` of length 1. Separator character.
#' @param times `scalar integer`. Number of times to repeat.
#'
#' @return `string`.
#'
#' @examples
#' cat(separator())
separator <- function(
    sep = c("\u2500", "=", "-", "+"),
    times = min(c(getOption("width", 72L), 72L))
) {
    sep <- match.arg(sep)
    assertIsAnImplicitInteger(times)
    paste0(rep(x = sep, times = times), collapse = "")
}
