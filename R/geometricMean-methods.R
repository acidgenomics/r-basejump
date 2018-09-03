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
#' @family Math and Science Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @inheritParams base::apply
#' @param removeNA `boolean`. Remove `NA` values from calculations.
#' @param zeroPropagate `boolean`. Allow propagation of zeroes.
#'
#' @return `numeric`.
#'
#' @seealso Modified version of `psych::geometric.mean()` and Paul McMurdie's
#'   [code](https://stackoverflow.com/a/25555105).
#'
#' @examples
#' # numeric ====
#' vec1 <- seq(1L, 5L, 1L)
#' print(vec1)
#' geometricMean(vec1)
#'
#' vec2 <- vec ^ 2L
#' print(vec2)
#' geometricMean(vec2)
#'
#' # matrix ====
#' matrix <- matrix(
#'     data = c(vec1, vec2),
#'     ncol = 2L,
#'     byrow = FALSE
#' )
#' print(matrix)
#' geometricMean(matrix)
#'
#' # sparseMatrix ====
#' sparse <- as(matrix, "sparseMatrix")
#' print(sparse)
#' geometricMean(sparse)
#'
#' # DataFrame ====
#' df <- as(matrix, "DataFrame")
#' print(df)
#' geometricMean(df)
NULL



#' @rdname geometricMean
#' @export
setMethod(
    f = "geometricMean",
    signature = signature("numeric"),
    definition = function(
        object,
        removeNA = TRUE,
        zeroPropagate = FALSE
    ) {
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
    f = "geometricMean",
    signature = signature("matrix"),
    definition = function(object, MARGIN = 2L) {
        invisible(lapply(object, assert_is_numeric))
        apply(
            X = object,
            MARGIN = MARGIN,
            FUN = geometricMean
        )
    }
)



#' @rdname geometricMean
#' @export
setMethod(
    f = "geometricMean",
    signature = signature("sparseMatrix"),
    definition = getMethod("geometricMean", "matrix")
)



#' @rdname geometricMean
#' @export
setMethod(
    f = "geometricMean",
    signature = signature("data.frame"),
    definition = getMethod("geometricMean", "matrix")
)



#' @rdname geometricMean
#' @export
setMethod(
    f = "geometricMean",
    signature = signature("DataFrame"),
    definition = getMethod("geometricMean", "matrix")
)
