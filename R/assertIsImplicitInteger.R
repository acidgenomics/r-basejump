#' Assert Is Implicit Integer
#'
#' @rdname assertIsImplicitInteger
#' @name assertIsImplicitInteger
#'
#' @family Assert Checks
#' @inherit assert
NULL



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsAnImplicitInteger(1)
assertIsAnImplicitInteger <- function(x) {
    stopifnot(isAnImplicitInteger(x))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsAnImplicitIntegerOrNULL(1)
#' assertIsAnImplicitIntegerOrNULL(NULL)
assertIsAnImplicitIntegerOrNULL <- function(x) {
    stopifnot(any(isAnImplicitInteger(x), is.null(x)))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsImplicitInteger(c(1, 2))
assertIsImplicitInteger <- function(x) {
    stopifnot(isImplicitInteger(x))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsImplicitIntegerOrNULL(c(1, 2))
#' assertIsImplicitIntegerOrNULL(NULL)
assertIsImplicitIntegerOrNULL <- function(x) {
    stopifnot(any(isImplicitInteger(x), is.null(x)))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' isAnImplicitInteger(1)
isAnImplicitInteger <- function(x) {
    if (!is_a_number(x)) {
        return(FALSE)
    }
    isImplicitInteger(x)
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' isImplicitInteger(list(1, 1L, 1.1, "XXX"))
isImplicitInteger <- function(x) {
    mapply(
        x = x,
        FUN = function(x) {
        if (!is.numeric(x)) {
            return(FALSE)
        }
        if (is.integer(x)) {
            return(TRUE)
        }
        isTRUE(all.equal(
            target = as.integer(x),
            current = x,
            tolerance = .Machine[["double.eps"]]
        ))
    },
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE)
}
