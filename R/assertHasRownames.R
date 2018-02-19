# TODO Use assert engine

#' Has Rownames Assert Check
#'
#' A stricter alternative to the assertive version that works properply with
#' data frames.
#'
#' @note `tibble::has_rownames()` works better than `assertive::has_rownames()`
#'   for data frames and tibbles.
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
assertHasRownames <- function(x, severity = "stop") {
    stopifnot(hasRownames(x))
    assert_are_disjoint_sets(
        x = rownames(x),
        y = as.character(seq_len(nrow(x))),
        severity = severity
    )
}



#' @rdname assertHasRownames
#' @export
hasRownames <- function(x) {  # nolint
    if (is.data.frame(x)) {
        tibble::has_rownames(x)
    } else {
        assertive::has_rownames(x)
    }
}



# Aliases ======================================================================
# Don't mask `assert_has_rownames()`
# Don't mask `has_rownames()`
