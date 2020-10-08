#' Interconvert log ratio and fold change values
#'
#' @name logRatio
#' @note Updated 2019-07-28.
#'
#' @inheritParams AcidRoxygen::params
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



## Updated 2019-07-22.
`foldChangeToLogRatio,numeric` <-  # nolint
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
    definition = `foldChangeToLogRatio,numeric`
)



## Updated 2019-07-22.
`logRatioToFoldChange,numeric` <-  # nolint
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
    definition = `logRatioToFoldChange,numeric`
)
