#' Assert Is URL
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsURL("https://steinbaugh.com")
assertIsURL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_character(x, severity = severity)
    assert_all_are_matching_regex(
        x = x,
        pattern = "^http(s)?\\://.+",
        severity = severity
    )
}
