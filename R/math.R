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
#' @param object Vector or column data (data frame, matrix).
#' @param na_rm Remove `NA` values before processing.
#'
#' @return Geometric means.
#' @export
#'
#' @examples
#' # Vector
#' vec <- seq(1,5)
#' vec2 <- vec^2
#' vec2[2] <- NA
#' geomean(vec)
#' geomean(vec2)
#'
#' # Data frame
#' df <- data.frame(vec, vec2)
#' geomean(df)
#' geomean(df, na_rm = FALSE)
geomean <- function(object, na_rm = TRUE) {
    if (is.vector(object)) {
        exp(mean(log(object), na.rm = TRUE))
    } else if (is.data.frame(object) | is.matrix(object)) {
        # `2` denotes columnwise calculation
        exp(apply(log(object), 2, mean, na.rm = na_rm))
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
    sprintf("%1.1f%%", number * 100)
}






# RNA-seq ====
#' log ratio to fold change
#'
#' Convert log ratio normalized values to fold change. Based on the approach
#' used in `gtools::logratio2foldchange()`.
#'
#' @param x Numeric vector of log ratio values.
#' @param base Logarithm base. Defaults to `2`, for compatibility with RNA-Seq
#'   differential expression output.
#'
#' @return Fold change values.
#' @export
#'
#' @examples
#' logRatioToFoldChange(seq(-3, 3, 0.5))
logRatioToFoldChange <- function(x, base = 2) {
    x <- base ^ x
    x <- ifelse(x < 1, -1 / x, x)
    x
}

#' @rdname snake_aliases
#' @usage NULL
#' @export
log_ratio_to_fold_change <- logRatioToFoldChange
