#' Integer Assert Check
#'
#' @inherit assert
#' @export
#'
#' @examples
#' # Explicit integer
#' assert_is_an_integer(1L)
#'
#' # Implicit integer
#' assert_is_an_integer(1, severity = "message")
assert_is_an_integer <- function(x, severity = "stop") {
    assert_is_a_number(x, severity = severity)
    assert_is_integer(x, severity = severity)
}
