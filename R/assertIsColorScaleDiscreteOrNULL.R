# TODO Need to add a check to distinguish `color/fill`

#' Is Color Palette Scale Discrete Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
#'
#' @examples
#' # Success
#' color <- scale_color_viridis(discrete = TRUE)
#' class(color)
#' assertIsColorScaleDiscreteOrNULL(color)
#' assertIsColorScaleDiscreteOrNULL(NULL)
#'
#' # Failure
#' color <- scale_color_viridis(discrete = FALSE)
#' class(color)
#' tryCatch(
#'     assertIsColorScaleDiscreteOrNULL(color),
#'     error = function(e) e)
assertIsColorScaleDiscreteOrNULL <- function(x, severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("ScaleDiscrete", "NULL"),
        severity = severity)
    if (!is.null(x)) {
        assert_is_all_of(
            x = x,
            classes = c("ggproto", "Scale", "ScaleDiscrete"),
            severity = severity)
    }
}



# Aliases ======================================================================
#' @rdname assertIsColorScaleDiscreteOrNULL
#' @export
assertIsColorScaleDiscreteOrNULL -> assert_is_color_scale_discrete_or_null
