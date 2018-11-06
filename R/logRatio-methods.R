#' @name logRatio
#' @inherit basejump.generics::logRatio
#'
#' @param base `scalar integer`. Logarithm base. Defaults to `2`, for
#'   compatibility with RNA-Seq differential expression output.
#'
#' @examples
#' ## Convert fold change to log ratio.
#' foldChangeToLogRatio(c(-8, -4, -2, 1, 2, 4, 8))
#'
#' ## Convert log ratio to fold change.
#' logRatioToFoldChange(seq(-3, 3, 1))
NULL



#' @importFrom basejump.generics foldChangeToLogRatio
#' @aliases NULL
#' @export
basejump.generics::foldChangeToLogRatio



#' @importFrom basejump.generics logRatioToFoldChange
#' @aliases NULL
#' @export
basejump.generics::logRatioToFoldChange



# foldChangeToLogRatio =========================================================
foldChangeToLogRatio.numeric <-  # nolint
    function(object, base = 2L) {
        assertIsAnImplicitInteger(base)
        base <- as.integer(base)
        assert_all_are_positive(base)
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



# logRatioToFoldChange =========================================================
logRatioToFoldChange.numeric <-  # nolint
    function(object, base = 2L) {
        assertIsAnImplicitInteger(base)
        base <- as.integer(base)
        assert_all_are_positive(base)
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
