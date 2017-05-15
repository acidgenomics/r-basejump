## General ====
#' Convert numeric to percentage.
#'
#' @param number Number.
#'
#' @return Percentage.
#' @export
#'
#' @examples
#' pct(0.1)
pct <- function(number) {
    return(sprintf("%1.1f%%", number * 100))
}






## RNA-seq counts ====
#' log ratio to fold change.
#'
#' Convert log ratio normalized values to fold change. Based on the approach
#' used in [gtools::logratio2foldchange()].
#'
#' @param logRatio Numeric vector of log ratio values.
#' @param base Logarithm base. Defaults to `2`, for compatibility with RNA-Seq
#'   differential expression output.
#'
#' @return Fold change values.
#' @export
#'
#' @examples
#' logRatioToFoldChange(seq(-3, 3, 0.5))
logRatioToFoldChange <- function(logRatio, base = 2) {
    x <- base^(logRatio)
    x <- ifelse(x < 1, -1/x, x)
    return(x)
}
