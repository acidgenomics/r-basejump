#' Geometric Mean
#'
#' The geometric mean is the nth root of n products or e to the mean log of `x`.
#' Useful for describing non-normal (i.e. geometric) distributions.
#'
#' @rdname geometricMean
#' @name geometricMean
#' @family Math Utilities
#'
#' @inheritParams AllGenerics
#'
#' @param removeNA Remove `NA` values from calculations.
#' @param zeroPropagate Allow propagation of zeroes.
#'
#' @details Modified version of `psych::geometric.mean()` and/or Paul McMurdie's
#'   code.
#'
#' @note This function should be fully zero- and `NA`-tolerant. This calculation
#'   is not particularly useful if there are elements that are <= 0 and will
#'   return `NaN`.
#'
#' @seealso [Paul McMurdie's post](https://stackoverflow.com/a/25555105) on
#'   Stack Overflow
#'
#' @return Numeric containing geometric means.
#'
#' @examples
#' # integer
#' vec <- seq(1L, 5L, 1L)
#' geometricMean(vec)
#' vec2 <- vec ^ 2L
#' geometricMean(vec2)
#'
#' # data.frame
#' df <- data.frame(vec, vec2)
#' geometricMean(df)
#'
#' # matrix
#' mat <- as.matrix(df)
#' geometricMean(mat)
NULL



# Constructors =================================================================
.geometricMean <- function(object, removeNA = TRUE, zeroPropagate = FALSE) {
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
        exp(sum(log(object[object > 0L]), na.rm = removeNA) / length(object))
    }
}



.geometricMeanDim <- function(object) {
    # Require that all columns are numeric (useful for data.frame)
    numericCol <- vapply(object, is.numeric, FUN.VALUE = logical(1L))
    if (!all(numericCol)) {
        # Return which columns aren't numeric
        nonnumericCol <- colnames(object)[!numericCol]
        abort(paste(
            "Non-numeric columns:", toString(nonnumericCol)
        ))
    }
    object %>%
        as.matrix() %>%
        # `2L` here denotes columnwise calculation
        apply(2L, .geometricMean)
}



# Methods ======================================================================
#' @rdname geometricMean
#' @export
setMethod(
    "geometricMean",
    signature("data.frame"),
    .geometricMeanDim)



#' @rdname geometricMean
#' @export
setMethod(
    "geometricMean",
    signature("integer"),
    .geometricMean)



#' @rdname geometricMean
#' @export
setMethod(
    "geometricMean",
    signature("matrix"),
    .geometricMeanDim)



#' @rdname geometricMean
#' @export
setMethod(
    "geometricMean",
    signature("numeric"),
    .geometricMean)
