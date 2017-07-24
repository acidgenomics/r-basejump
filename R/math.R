#' Geometric Mean
#'
#' The geometric mean is the nth root of n products or e to the mean log of `x`.
#' Useful for describing non-normal (i.e. geometric) distributions.
#'
#' @family Math Utilities
#'
#' @details Modified version of `psych::geometric.mean()`.
#'
#' @note Not particularly useful if there are elements that are <= 0.
#'
#' @param x Vector or column data (data frame, matrix).
#'
#' @return Geometric means.
#' @export
#'
#' @examples
#' # Vector
#' vec <- seq(1L, 5L, 1L)
#' geomean(vec)
#'
#' vec2 <- vec ^ 2L
#' geomean(vec2)
#'
#' # Data frame
#' df <- data.frame(vec, vec2)
#' geomean(df)
geomean <- function(x) {
    if (is.vector(x)) {
        exp(mean(log(x), na.rm = TRUE))
    } else if (is.data.frame(x) | is.matrix(x)) {
        # `2` denotes columnwise calculation
        exp(apply(log(x), 2L, mean, na.rm = TRUE))
    }
}



#' Convert Numeric to Percentage
#'
#' @family Math Utilities
#'
#' @param number Number.
#'
#' @return Percentage.
#' @export
#'
#' @examples
#' pct(0.1)
pct <- function(number) {
    sprintf("%1.1f%%", number * 100L)
}
