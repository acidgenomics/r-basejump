#' Assert Has Rownames
#'
#' A stricter alternative to the assertive version that works properply with
#' data frames.
#'
#' @note `tibble::has_rownames()` works better than `assertive::has_rownames()`
#'   for data frames and tibbles.
#'
#' @family Assert Check Functions
#' @author Michael Steinbaugh
#' @inherit assert
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'     "sample_1" = c(1L, 2L),
#'     "sample_2" = c(3L, 4L),
#'     row.names = c("gene_1", "gene_2"),
#'     stringsAsFactors = FALSE
#' )
#' assertHasRownames(data)
assertHasRownames <- function(
    x,
    severity = getOption("assertive.severity", "stop")
) {
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
