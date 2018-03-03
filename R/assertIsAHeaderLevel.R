#' Assert Is a Header Level
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @export
#'
#' @examples
#' # Markdown supports levels 1-7
#' assertIsAHeaderLevel(1L)
#' tryCatch(
#'     assertIsAHeaderLevel(8L),
#'     error = function(e) e
#' )
assertIsAHeaderLevel <- function(x, severity = "stop") {
    assert_is_a_number(x, severity = severity)
    assert_is_subset(
        x = as.integer(x),
        y = seq(1L:7L),
        severity = severity
    )
}
