#' Interconvert Log Ratio and Fold Change Values
#'
#' @rdname logRatio
#' @name logRatio
#' @family Math Utilities
#'
#' @inheritParams AllGenerics
#'
#' @param object Numeric of log ratio or fold change values.
#' @param base Logarithm base. Defaults to `2`, for compatibility with RNA-Seq
#'   differential expression output.
#'
#' @return [numeric].
#'
#' @seealso Modified variants of `gtools::foldchange2logratio()` and
#' `gtools::logratio2foldchange()`.
#'
#' @examples
#' # Convert log ratio to fold change
#' logRatioToFoldChange(seq(-3, 3, 1))
#'
#' # Convert fold change to log ratio
#' foldChangeToLogRatio(c(-8, -4, -2, 1, 2, 4, 8))
NULL



# Methods ======================================================================
#' @rdname logRatio
setMethod(
    "foldChangeToLogRatio",
    signature("numeric"),
    function(object, base = 2L) {
        .checkBase(base)
        object <- ifelse(object < 0L, 1L / -object, object)
        object <- log(object, base)
        object
    })
