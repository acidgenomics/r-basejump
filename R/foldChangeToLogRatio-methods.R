#' Interconvert Log Ratio and Fold Change Values
#'
#' @name logRatio
#' @family Math and Science Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param base `scalar integer`. Logarithm base. Defaults to `2`, for
#'   compatibility with RNA-Seq differential expression output.
#'
#' @return `numeric`.
#'
#' @seealso Modified variants of:
#' - `gtools::foldchange2logratio()`.
#' - `gtools::logratio2foldchange()`.
#'
#' @examples
#' # Convert log ratio to fold change
#' logRatioToFoldChange(seq(-3, 3, 1))
#'
#' # Convert fold change to log ratio
#' foldChangeToLogRatio(c(-8, -4, -2, 1, 2, 4, 8))
NULL



#' @rdname logRatio
#' @export
setMethod(
    f = "foldChangeToLogRatio",
    signature = signature("numeric"),
    definition = function(object, base = 2L) {
        assertIsAnImplicitInteger(base)
        base <- as.integer(base)
        assert_all_are_positive(base)
        object <- ifelse(object < 0L, 1L / -object, object)
        object <- log(object, base)
        object
    }
)
