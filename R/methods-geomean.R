#' Geometric Mean
#'
#' The geometric mean is the nth root of n products or e to the mean log of `x`.
#' Useful for describing non-normal (i.e. geometric) distributions.
#'
#' @rdname geomean
#' @name geomean
#'
#' @details Modified version of `psych::geometric.mean()`.
#'
#' @note Not particularly useful if there are elements that are <= 0.
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
.geomeanDim <- function(object) {
    if (is.null(dim(object))) stop("Object must support dim()")
    object %>%
        as.matrix %>%
        log %>%
        # `2` denotes columnwise calculation
        apply(2L, mean, na.rm = TRUE) %>%
        exp
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
setMethod("geomean", "numeric", function(object) {
    object %>%
        log %>%
        mean(na.rm = TRUE) %>%
        exp
})
