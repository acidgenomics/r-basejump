#' Interconvert log ratio and fold change values
#'
#' @name logRatio
#'
#' @inheritParams params
#' @param base `integer(1)`.
#'   Logarithm base. Defaults to `2`, for compatibility with RNA-Seq
#'   differential expression output.
#' @param ... Additional arguments.
#'
#' @seealso
#' - `gtools::foldchange2logratio()`.
#' - `gtools::logratio2foldchange()`.
#'
#' @return `numeric`.
#'
#' @examples
#' ## Convert fold change to log ratio.
#' foldChangeToLogRatio(c(-8, -4, -2, 1, 2, 4, 8))
#'
#' ## Convert log ratio to fold change.
#' logRatioToFoldChange(seq(-3, 3, 1))
NULL



#' @rdname logRatio
#' @name foldChangeToLogRatio
#' @importFrom bioverbs foldChangeToLogRatio
#' @usage foldChangeToLogRatio(object, ...)
#' @export
NULL

#' @rdname logRatio
#' @name logRatioToFoldChange
#' @importFrom bioverbs logRatioToFoldChange
#' @usage logRatioToFoldChange(object, ...)
#' @export
NULL



## foldChangeToLogRatio ========================================================
foldChangeToLogRatio.numeric <-  # nolint
    function(object, base = 2L) {
        assert(isInt(base), isPositive(base))
        base <- as.integer(base)
        object <- ifelse(object < 0L, 1L / -object, object)
        object <- log(object, base)
        object
    }



#' @rdname logRatio
#' @export
setMethod(
    f = "foldChangeToLogRatio",
    signature = signature("numeric"),
    definition = foldChangeToLogRatio.numeric
)



## logRatioToFoldChange ========================================================
logRatioToFoldChange.numeric <-  # nolint
    function(object, base = 2L) {
        assert(isInt(base), isPositive(base))
        base <- as.integer(base)
        object <- base ^ object
        object <- ifelse(object < 1L, -1L / object, object)
        object
    }



#' @rdname logRatio
#' @export
setMethod(
    f = "logRatioToFoldChange",
    signature = signature("numeric"),
    definition = logRatioToFoldChange.numeric
)
