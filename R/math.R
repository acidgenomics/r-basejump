# General ====
#' Geometric mean
#'
#' The geometric mean is the nth root of n products or e to the mean log of `x`.
#' Useful for describing non-normal (i.e. geometric) distributions.
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



#' Convert numeric to percentage
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






# RNA-seq ====
#' Interconvert log ratio and fold change values
#'
#' @rdname logRatio
#'
#' @param x Numeric vector of log ratio (`lr`) or fold change (`fc`) values.
#' @param base Logarithm base. Defaults to `2`, for compatibility with RNA-Seq
#'   differential expression output.
#'
#' @return Numeric vector.
#' @export
#'
#' @seealso
#' Modified variants of `gtools::foldchange2logratio()` and
#' `gtools::logratio2foldchange()`.
#'
#' @examples
#' lr2fc(seq(-3, 3, 1))
#' fc2lr(c(-8, -4, -2, 1, 2, 4, 8))
fc2lr <- function(x, base = 2L) {
    x <- ifelse(x < 0L, 1L / -x, x)
    x <- log(x, base)
    x
}



#' @rdname logRatio
#' @export
lr2fc <- function(x, base = 2L) {
    x <- base ^ x
    x <- ifelse(x < 1L, -1L / x, x)
    x
}
