# TODO Need to add a check to distinguish `color/fill`

#' Assert Is Color Palette Scale Continuous or NULL
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
#'
#' @examples
#' # Success
#' color <- scale_color_viridis(discrete = FALSE)
#' class(color)
#' assertIsColorScaleContinuousOrNULL(color)
#' assertIsColorScaleContinuousOrNULL(NULL)
#'
#' # Failure
#' color <- scale_color_viridis(discrete = TRUE)
#' class(color)
#' tryCatch(
#'     assertIsColorScaleContinuousOrNULL(color),
#'     error = function(e) e)
assertIsColorScaleContinuousOrNULL <- function(x, severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("ScaleContinuous", "NULL"),
        severity = severity)
    if (!is.null(x)) {
        assert_is_all_of(
            x = x,
            classes = c("ggproto", "Scale", "ScaleContinuous"),
            severity = severity)
    }
}
