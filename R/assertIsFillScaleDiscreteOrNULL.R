# TODO Need to add a check to distinguish `color/fill`

#' Assert Is Fill Palette Scale Discrete or NULL
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @export
#'
#' @examples
#' # Success
#' fill <- scale_color_viridis(discrete = TRUE)
#' class(fill)
#' assertIsFillScaleDiscreteOrNULL(fill)
#' assertIsFillScaleDiscreteOrNULL(NULL)
#'
#' # Failure
#' fill <- scale_color_viridis(discrete = FALSE)
#' class(fill)
#' tryCatch(
#'     assertIsFillScaleDiscreteOrNULL(color),
#'     error = function(e) e
#' )
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
