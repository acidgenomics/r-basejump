#' Assert Is Character Vector or NULL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsCharacterOrNULL(c("hello", "world"))
#' assertIsCharacterOrNULL(NULL)
assertIsCharacterOrNULL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_any_of(
        x = x,
        classes = c("character", "NULL"),
        severity = severity
    )
}
