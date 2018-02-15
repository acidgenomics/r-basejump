#' Implict Integer Assert Check
#'
#' @family Assert Checks
#' @rdname assert_is_implicit_integer
#' @name assert_is_implicit integer
#'
#' @inherit assert
#' @inheritParams general
NULL



#' @rdname assert_is_implicit_integer
#' @export
assert_is_an_implicit_integer <- function(x) {  # nolint
    assert_is_implicit_integer(x)
    assert_is_scalar(x)
}



#' @rdname assert_is_implicit_integer
#' @export
assert_is_an_implicit_integer_or_null <- function(x) {  # nolint
    assert_is_implicit_integer_or_null(x)
    if (is_implicit_integer(x)) {
        assert_is_a_number(x)
    }
}



#' @rdname assert_is_implicit_integer
#' @export
assert_is_implicit_integer <- function(x) {  # nolint
    stopifnot(is_implicit_integer(x))
}



#' @rdname assert_is_implicit_integer
#' @export
assert_is_implicit_integer_or_null <- function(x) {  # nolint
    stopifnot(any(is_implicit_integer(x), is.null(x)))
}



#' @rdname assert_is_implicit_integer
#' @seealso [base::all.equal()] `tolerance` parameter.
#' @export
is_implicit_integer <- function(x) {  # nolint
    if (!is.numeric(x)) {
        return(FALSE)
    }
    if (is.integer(x)) {
        return(TRUE)
    }
    isTRUE(all.equal(x, as.integer(x), tolerance = .Machine[["double.eps"]]))
}
