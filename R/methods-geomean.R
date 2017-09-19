#' Geometric Mean
#'
#' The geometric mean is the nth root of n products or e to the mean log of `x`.
#' Useful for describing non-normal (i.e. geometric) distributions.
#'
#' @rdname geomean
#' @name geomean
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
#' # Vector
#' vec <- seq(1L, 5L, 1L)
#' geomean(vec)
#'
#' vec2 <- vec ^ 2L
#' geomean(vec2)
#'
#'
#' # Data frame
#' df <- data.frame(vec, vec2)
#' geomean(df)
#'
#'
#' # Matrix
#' mat <- as.matrix(df)
#' geomean(mat)
NULL



# Constructors ====
.geomean <- function(object, removeNA = TRUE, zeroPropagate = FALSE) {
    # Check for any negative numbers and return `NaN`
    if (any(object < 0L, na.rm = TRUE)) {
        warning("'geomean()' returns 'NaN' when negative numbers are present")
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

.geomeanDim <- function(object) {
    if (is.null(dim(object))) stop("Object must support dim()")
    object %>%
        as.matrix %>%
        # `2L` here denotes columnwise calculation
        apply(2L, .geomean)
}



# Methods ====
#' @rdname geomean
#' @export
setMethod("geomean", "ANY", .geomeanDim)



#' @rdname geomean
#' @export
setMethod("geomean", "data.frame", .geomeanDim)



#' @rdname geomean
#' @export
setMethod("geomean", "matrix", .geomeanDim)



#' @rdname geomean
#' @export
setMethod("geomean", "numeric", .geomean)
