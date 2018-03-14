#' Assert Is a Header Level
#'
#' Markdown supports header levels 1-7 (`<H1>`-`<H7>`).
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsAHeaderLevel(1L)
assertIsAHeaderLevel <- function(x, severity = "stop") {
    assert_is_a_number(x, severity = severity)
    assert_is_subset(
        x = as.integer(x),
        y = seq(1L:7L),
        severity = severity
    )
}
