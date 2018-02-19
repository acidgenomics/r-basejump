#' Data Frame or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
#'
#' @examples
#' # Success
#' assertIsDataFrameOrNULL(mtcars)
#' assertIsDataFrameOrNULL(NULL)
#'
#' # Failure
#' tryCatch(
#'     assertIsDataFrameOrNULL(1L),
#'     error = function(e) e)
assertIsDataFrameOrNULL <- function(x, severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("data.frame", "NULL"),
        severity = severity)
}



# Aliases ======================================================================
#' @rdname assertIsDataFrameOrNULL
#' @export
assertIsDataFrameOrNULL -> assert_is_data.frame_or_null
