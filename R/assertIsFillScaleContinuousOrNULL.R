# TODO Need to add a check to distinguish `color/fill`

#' Assert Is Fill Palette Scale Continuous or NULL
#'
#' @family Assert Check Functions
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
#'     error = function(e) e
#' )
assertIsFillScaleContinuousOrNULL <- function(x, severity = "stop") {
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
