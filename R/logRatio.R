#' Interconvert Log Ratio and Fold Change Values
#'
#' @rdname logRatio
#' @family Math Utilities
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
