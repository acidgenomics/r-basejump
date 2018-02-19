# TODO Need to add a check to distinguish `color/fill`

#' Is Fill Palette Scale Continuous or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
#'
#' @examples
#' # Success
#' fill <- scale_fill_viridis(discrete = FALSE)
#' class(fill)
#' assertIsFillScaleContinuousOrNULL(fill)
#' assertIsFillScaleContinuousOrNULL(NULL)
#'
#' # Failure
#' fill <- scale_color_viridis(discrete = TRUE)
#' class(fill)
#' tryCatch(
#'     assertIsFillScaleContinuousOrNULL(color),
#'     error = function(e) e)
assertIsFillScaleContinuousOrNULL <- function(x, severity = "stop") {
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



# Aliases ======================================================================
#' @rdname assertIsFillScaleContinuousOrNULL
#' @export
assertIsFillScaleContinuousOrNULL -> assert_is_fill_scale_continuous_or_null
