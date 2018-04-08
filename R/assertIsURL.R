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



#' @rdname assertIsURL
#' @export
isURL <- function(x) {
    if (!is.character(x)) {
        return(FALSE)
    }
    vapply(
        X = x,
        FUN = function(x) {
            grepl("^http(s)?\\://.+", x)
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    )
}
