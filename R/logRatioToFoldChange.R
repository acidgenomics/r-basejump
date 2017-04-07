#' log ratio to fold change
#'
#' Convert log ratio normalized values to fold change.
#' Based on the approach used in `gtools::logratio2foldchange()`.
#'
#' @author Michael Steinbaugh
#'
#' @param logRatio Numeric vector of log ratio values
#' @param base Logarithm base. Defaults to \code{2}, for compatibility with
#'   RNA-Seq differential expression output.
#'
#' @return Fold change values
#' @export
#'
#' @examples
#' logRatioToFoldChange(seq(-3, 3, 0.5))
logRatioToFoldChange <- function(logRatio, base = 2) {
    x <- base^(logRatio)
    x <- ifelse(x < 1, -1/x, x)
    return(x)
}



# Alternate approach
# sign is -1 if log2 < 0; 1 if log2 >= 0
# sign <- (-1) ^ (1 + as.numeric(log2 >= 0))
# return(sign * 2 ^ abs(log2))
