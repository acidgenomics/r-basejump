# TODO Use assert engine

#' Implicit Integer Assert Check
#'
#' @rdname assertIsImplicitInteger
#' @name assertIsImplicitInteger
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
NULL



#' @rdname assertIsImplicitInteger
#' @export
assertIsAnImplicitInteger <- function(x) {
    assert_is_implicit_integer(x)
    assert_is_scalar(x)
}



#' @rdname assertIsImplicitInteger
#' @export
assertIsAnImplicitIntegerOrNULL <- function(x) {
    assert_is_implicit_integer_or_null(x)
    if (is_implicit_integer(x)) {
        assert_is_a_number(x)
    }
}



#' @rdname assertIsImplicitInteger
#' @export
assertIsImplicitInteger <- function(x) {
    stopifnot(is_implicit_integer(x))
}



#' @rdname assertIsImplicitInteger
#' @export
assertIsImplicitIntegerOrNULL <- function(x) {
    stopifnot(any(is_implicit_integer(x), is.null(x)))
}



#' @rdname assertIsImplicitInteger
#' @export
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

#' @rdname isImplicitInteger
#' @export
isImplicitInteger -> is_implicit_integer
