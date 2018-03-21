#' Assert Is Fill Palette Scale Discrete or NULL
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @export
#'
#' @examples
#' fill <- scale_color_viridis(discrete = TRUE)
#' class(fill)
#' assertIsFillScaleDiscreteOrNULL(fill)
#' assertIsFillScaleDiscreteOrNULL(NULL)
assertIsFillScaleDiscreteOrNULL <- function(x, severity = "stop") {
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
