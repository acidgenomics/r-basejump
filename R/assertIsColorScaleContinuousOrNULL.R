#' Assert Is Color Palette Scale Continuous or NULL
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @export
#'
#' @examples
#' color <- scale_color_viridis(discrete = FALSE)
#' class(color)
#' assertIsColorScaleContinuousOrNULL(color)
#' assertIsColorScaleContinuousOrNULL(NULL)
assertIsColorScaleContinuousOrNULL <- function(x, severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("ScaleContinuous", "NULL"),
        severity = severity
    )
    if (!is.null(x)) {
        assert_is_all_of(
            x = x,
            classes = c("ggproto", "Scale", "ScaleContinuous"),
            severity = severity
        )
    }
}
