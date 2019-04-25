#' @name geometricMean
#' @inherit bioverbs::geometricMean
#'
#' @inheritParams params
#' @inheritParams base::apply
#' @param removeNA `logical(1)`.
#'   Remove `NA` values from calculations.
#' @param zeroPropagate `logical(1)`.
#'   Allow propagation of zeroes.
#' @param ... Additional arguments.
#'
#' @note This function should be fully zero- and `NA`-tolerant. This calculation
#'   is not particularly useful if there are elements that are <= 0 and will
#'   return `NaN`.
#'
#' @seealso
#' - [Paul McMurdie's code](https://stackoverflow.com/a/25555105).
#' - `psych::geometric.mean()`.
#'
#' @return `numeric`.
#'
#' @examples
#' ## numeric ====
#' vec1 <- seq(1L, 5L, 1L)
#' print(vec1)
#' geometricMean(vec1)
#'
#' vec2 <- vec1 ^ 2L
#' print(vec2)
#' geometricMean(vec2)
#'
#' ## matrix ====
#' matrix <- matrix(
#'     data = c(vec1, vec2),
#'     ncol = 2L,
#'     byrow = FALSE
#' )
#' print(matrix)
#' geometricMean(matrix)
#'
#' ## sparseMatrix ====
#' sparse <- as(matrix, "sparseMatrix")
#' print(sparse)
#' geometricMean(sparse)
NULL



#' @rdname geometricMean
#' @name geometricMean
#' @importFrom bioverbs geometricMean
#' @usage geometricMean(object, ...)
#' @export
NULL



geometricMean.numeric <-  # nolint
    function(
        object,
        removeNA = TRUE,
        zeroPropagate = FALSE
    ) {
        assert(
            isFlag(removeNA),
            isFlag(zeroPropagate)
        )

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



#' @rdname geometricMean
#' @export
setMethod(
    f = "geometricMean",
    signature = signature("numeric"),
    definition = geometricMean.numeric
)



geometricMean.matrix <-  # nolint
    function(object, MARGIN = 2L) {  # nolint
        apply(
            X = object,
            MARGIN = MARGIN,
            FUN = geometricMean
        )
    }



#' @rdname geometricMean
#' @export
setMethod(
    f = "geometricMean",
    signature = signature("matrix"),
    definition = geometricMean.matrix
)



geometricMean.sparseMatrix <-  # nolint
    geometricMean.matrix

#' @rdname geometricMean
#' @export
setMethod(
    f = "geometricMean",
    signature = signature("sparseMatrix"),
    definition = geometricMean.sparseMatrix
)
