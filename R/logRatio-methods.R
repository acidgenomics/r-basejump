#' Interconvert Log Ratio and Fold Change Values
#'
#' @rdname logRatio
#' @name logRatio
#' @family Math Utilities
#'
#' @param object Numeric vector of log ratio (`lr`) or fold change (`fc`)
#'   values.
#' @param base Logarithm base. Defaults to `2`, for compatibility with RNA-Seq
#'   differential expression output.
#'
#' @return Numeric vector.
#'
#' @seealso
#' Modified variants of `gtools::foldchange2logratio()` and
#' `gtools::logratio2foldchange()`.
#'
#' @examples
#' lr2fc(seq(-3, 3, 1))
#' fc2lr(c(-8, -4, -2, 1, 2, 4, 8))
NULL



#' @rdname logRatio
setMethod("fc2lr", "numeric", function(object, base = 2L) {
    object <- ifelse(object < 0L, 1L / -object, object)
    object <- log(object, base)
    object
})



#' @rdname logRatio
#' @export
setMethod("lr2fc", "numeric", function(object, base = 2L) {
    object <- base ^ object
    object <- ifelse(object < 1L, -1L / object, object)
    object
})
