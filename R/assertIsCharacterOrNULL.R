#' Assert Is Character Vector or NULL
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
#'
#' @examples
#' # Success
#' assertIsCharacterOrNULL(c("hello", "world"))
#' assertIsCharacterOrNULL(NULL)
#'
#' # Failure
#' tryCatch(
#'     assertIsCharacterOrNULL(1L),
#'     error = function(e) e)
assertIsCharacterOrNULL <- function(x, severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("character", "NULL"),
        severity = severity)
}
