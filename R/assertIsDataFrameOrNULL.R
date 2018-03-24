#' Assert Is Data Frame or NULL
#'
#' @note This checks for `data.frame` and will stop for `DataFrame` class.
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsDataFrameOrNULL(mtcars)
#' assertIsDataFrameOrNULL(NULL)
assertIsDataFrameOrNULL <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
    assert_is_any_of(
        x = x,
        classes = c("data.frame", "NULL"),
        severity = severity
    )
}
