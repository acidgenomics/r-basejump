#' Geometric Mean
#'
#' The geometric mean is the nth root of n products or e to the mean log of `x`.
#' Useful for describing non-normal (i.e. geometric) distributions.
#'
#' This function should be fully zero- and `NA`-tolerant. This calculation is
#' not particularly useful if there are elements that are <= 0 and will return
#' `NaN`.
#'
#' @name geometricMean
#' @family Math Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param removeNA Remove `NA` values from calculations.
#' @param zeroPropagate Allow propagation of zeroes.
#'
#' @return `numeric` containing geometric means.
#'
#' @seealso Modified version of `psych::geometric.mean()` and Paul McMurdie's
#'   [code](https://stackoverflow.com/a/25555105).
#'
#' @examples
#' # numeric ====
#' vec <- seq(1L, 5L, 1L)
#' geometricMean(vec)
#' vec2 <- vec ^ 2L
#' geometricMean(vec2)
#'
#' # data.frame ====
#' df <- data.frame(vec, vec2)
#' geometricMean(df)
#'
#' # matrix ====
#' mat <- as.matrix(df)
#' geometricMean(mat)
NULL



# Methods ======================================================================
#' @rdname geometricMean
#' @export
setMethod(
    "geometricMean",
    signature("numeric"),
    function(object, removeNA = TRUE, zeroPropagate = FALSE) {
        assert_is_a_bool(removeNA)
        assert_is_a_bool(zeroPropagate)

        # Check for any negative numbers and return `NaN`
        if (any(object < 0L, na.rm = TRUE)) {
            return(NaN)
        }

        if (isTRUE(zeroPropagate)) {
            if (any(object == 0L, na.rm = TRUE)) {
                return(0L)
            }
            exp(mean(log(object), na.rm = removeNA))
        } else {
            exp(
                sum(log(object[object > 0L]), na.rm = removeNA) /
                    length(object)
            )
        }
    }
)



#' @rdname geometricMean
#' @export
setMethod(
    "geometricMean",
    signature("matrix"),
    function(object) {
        invisible(lapply(object, assert_is_numeric))
        apply(
            X = object,
            MARGIN = 2L,
            FUN = .geometricMean
        )
    }
)



#' @rdname geometricMean
#' @export
setMethod(
    "geometricMean",
    signature("data.frame"),
    getMethod("geometricMean", "matrix")
)
