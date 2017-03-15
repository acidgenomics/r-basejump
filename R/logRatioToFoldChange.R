# Based on `gtools::logratio2foldchange()` method


#' log ratio to fold change
#'
#' Convert log ratio normalized values to fold change
#'
#' @author Michael Steinbaugh
#' @keywords math rnaseq
#'
#' @param logRatio \code{numeric} vector of log ratio values
#' @param base Logarithm base. Defaults to 2, for compatibility with RNA-Seq
#'   differential expression output.
#'
#' @return Fold change values
#' @export
#'
#' @examples
#' logRatioToFoldChange(seq(-3, 3, 0.5))
logRatioToFoldChange <- function(logRatio, base = 2) {
    return <- base^(logRatio)
    return <- ifelse(return < 1, -1/return, return)
    return(return)
}


# Deprecated method
# Sign is -1 if log2 < 0; 1 if log2 >= 0
## sign <- (-1) ^ (1 + as.numeric(log2 >= 0))
## sign * 2 ^ abs(log2)
