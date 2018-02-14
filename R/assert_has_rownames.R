#' Has Rownames Assert Check
#'
#' A stricter alternative to the assertive version that works properply with
#' data frames.
#'
#' @note `tibble::has_rownames()` works better than `assertive::has_rownames()`
#'   for data frames and tibbles.
#'
#' @family Assert Checks
#'
#' @inherit assert
#' @inheritParams general
#'
#' @export
assert_has_rownames <- function(x) {  # nolint
    stopifnot(has_rownames(x))
    assert_are_disjoint_sets(
        rownames(x),
        as.character(seq_len(nrow(x)))
    )
}



#' @rdname assert_has_rownames
#' @export
has_rownames <- function(x) {  # nolint
    if (is.data.frame(x)) {
        tibble::has_rownames(x)
    } else {
        assertive::has_rownames(x)
    }
}
