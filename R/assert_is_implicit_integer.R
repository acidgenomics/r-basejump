#' Implict Integer Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
assert_is_implicit_integer <- function(x) {  # nolint
    stopifnot(is_implicit_integer(x))
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
