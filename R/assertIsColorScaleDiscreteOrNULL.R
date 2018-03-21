#' Assert Is Color Palette Scale Discrete or NULL
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @export
#'
#' @examples
#' color <- scale_color_viridis(discrete = TRUE)
#' class(color)
#' assertIsColorScaleDiscreteOrNULL(color)
#' assertIsColorScaleDiscreteOrNULL(NULL)
assertIsColorScaleDiscreteOrNULL <- function(x, severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("ScaleDiscrete", "NULL"),
        severity = severity
    )
    if (!is.null(x)) {
        assert_is_all_of(
            x = x,
            classes = c("ggproto", "Scale", "ScaleDiscrete"),
            severity = severity
        )
    }
}
