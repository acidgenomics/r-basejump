# TODO Use assert engine

#' Assert Has Rownames
#'
#' A stricter alternative to the assertive version that works properply with
#' data frames.
#'
#' @note `tibble::has_rownames()` works better than `assertive::has_rownames()`
#'   for data frames and tibbles.
#'
#' @family Assert Check Functions
#' @inherit assert
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'     sample1 = c(1L, 2L),
#'     sample2 = c(3L, 4L),
#'     row.names = c("gene1", "gene2"),
#'     stringsAsFactors = FALSE)
#' assertHasRownames(data)
#'
#' # Now set the rownames as NULL
#' rownames(data) <- NULL
#' tryCatch(
#'     assertHasRownames(data),
#'     error = function(e) e)
#'
#' tibble <- tibble(
#'     sample1 = c(1L, 2L),
#'     sample2 = c(3L, 4L)
#' )
#' tryCatch(
#'     assertHasRownames(tibble),
#'     error = function(e) e)
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
