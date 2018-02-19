# TODO Use assert engine

#' Implicit Integer Assert Check
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
    assert_is_a_number(x)
    assert_is_implicit_integer(x)
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsAnImplicitIntegerOrNULL(1)
#' assertIsAnImplicitIntegerOrNULL(NULL)
assertIsAnImplicitIntegerOrNULL <- function(x) {
    assertIsImplicitIntegerOrNULL(x)
    if (isImplicitInteger(x)) {
        assert_is_a_number(x)
    }
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
#' isImplicitInteger(1)
#' # Also returns `TRUE` for explicit integers
#' isImplicitInteger(1L)
isImplicitInteger <- function(x) {
    if (!is.numeric(x)) {
        return(FALSE)
    }
    if (is.integer(x)) {
        return(TRUE)
    }
    isTRUE(all.equal(x, as.integer(x), tolerance = .Machine[["double.eps"]]))
}



# Aliases ======================================================================
#' @rdname assertIsImplicitInteger
#' @export
assertIsAnImplicitInteger -> assert_is_an_implicit_integer

#' @rdname assertIsImplicitInteger
#' @export
assertIsAnImplicitIntegerOrNULL -> assert_is_an_implicit_integer_or_null

#' @rdname assertIsImplicitInteger
#' @export
assertIsImplicitInteger -> assert_is_implicit_integer

#' @rdname assertIsImplicitInteger
#' @export
assertIsImplicitIntegerOrNULL -> assert_is_implicit_integer_or_null

#' @rdname assertIsImplicitInteger
#' @export
isImplicitInteger -> is_implicit_integer
