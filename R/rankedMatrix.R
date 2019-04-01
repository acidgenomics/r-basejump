#' Calculate a ranked matrix
#'
#' @note Ties are resolved automatically by calculating the average. See the
#'   `ties.method` parameter in [`rank()]][base::rank] for details.
#'
#' @export
#' @inheritParams params
#' @inheritParams base::apply
#'
#' @param method `character(1)`.
#'   Rank the values in `increasing`, `decreasing`, or `bidirectional` order.
#'
#' @seealso
#' - [`rank()`][base::rank].
#' - Not to be confused with [Matrix::rankMatrix()].
#'
#' @examples
#' ## Order was randomized using `sample()`.
#' mat <- matrix(
#'     data = c(
#'         -2.00, -1.25,  0.00,  0.25,  0.25,  0.50,  2.00,  4.00,
#'          2.00,  0.25, -1.25, -2.00,  0.50,  0.25,  4.00,  0.00,
#'          0.50,  4.00, -1.25,  0.25,  0.00,  0.25,  2.00, -2.00,
#'          4.00,  0.50, -2.00,  2.00,  0.25, -1.25,  0.25,  0.00
#'     ),
#'     nrow = 8L,
#'     ncol = 4L,
#'     byrow = FALSE,
#'     dimnames = list(
#'         paste0("gene", seq_len(8L)),
#'         paste0("contrast", seq_len(4L))
#'     )
#' )
rankedMatrix <- function(
    object,
    MARGIN = 2L,  # nolint
    method = c("increasing", "decreasing", "bidirectional")
) {
    assert(
        is.matrix(object),
        isInt(MARGIN)
    )
    method <- match.arg(method)

    if (method %in% c("decreasing", "increasing")) {
        if (method == "decreasing") {
            decreasing <- TRUE
        } else if (method == "increasing") {
            decreasing <- FALSE
        }
        mat <- .rank.matrix(
            x = object,
            MARGIN = MARGIN,
            decreasing = decreasing
        )
    } else if (method == "bidirectional") {
        mat <- .bidirRank.matrix(
            x = object,
            MARGIN = MARGIN,
            removeZeros = TRUE
        )
    }

    mat
}



# Consider using `data.table::frank()` instead of `base::rank()` for speed.
# This adds an additional dependency, so avoid at the moment.

# nolint start
#
# x
# > gene1 gene2 gene3 gene4 gene5 gene6 gene7 gene8
# >  2.00  0.25 -1.25 -2.00  0.50  0.25  4.00  0.00
#
# .rank.numeric(x, decreasing = FALSE)
# > gene1 gene2 gene3 gene4 gene5 gene6 gene7 gene8
# >   7.0   4.5   2.0   1.0   6.0   4.5   8.0   3.0
#
# .rank.numeric(x, decreasing = TRUE)
# > gene1 gene2 gene3 gene4 gene5 gene6 gene7 gene8
# >   2.0   4.5   7.0   8.0   3.0   4.5   1.0   6.0
#
# nolint end

.rank.numeric <- function(x, decreasing = FALSE) {
    assert(
        is.numeric(x),
        isFlag(decreasing)
    )
    r <- x
    if (isTRUE(decreasing)) r <- -r
    rank(r, na.last = TRUE, ties.method = "average")
}



# nolint start
#
# x
# >       contrast1 contrast2 contrast3 contrast4
# > gene1     -2.00      2.00      0.50      4.00
# > gene2     -1.25      0.25      4.00      0.50
# > gene3      0.00     -1.25     -1.25     -2.00
# > gene4      0.25     -2.00      0.25      2.00
# > gene5      0.25      0.50      0.00      0.25
# > gene6      0.50      0.25      0.25     -1.25
# > gene7      2.00      4.00      2.00      0.25
# > gene8      4.00      0.00     -2.00      0.00
#
# .rank.matrix(x, MARGIN = 2L, decreasing = FALSE)
# >       contrast1 contrast2 contrast3 contrast4
# > gene1       1.0       7.0       6.0       8.0
# > gene2       2.0       4.5       8.0       6.0
# > gene3       3.0       2.0       2.0       1.0
# > gene4       4.5       1.0       4.5       7.0
# > gene5       4.5       6.0       3.0       4.5
# > gene6       6.0       4.5       4.5       2.0
# > gene7       7.0       8.0       7.0       4.5
# > gene8       8.0       3.0       1.0       3.0
#
# nolint end

.rank.matrix <- function(
    x,
    MARGIN = 2L,  # nolint
    decreasing = FALSE
) {
    assert(
        is.matrix(x),
        isFlag(decreasing),
        isInt(MARGIN)
    )
    apply(X = x, MARGIN = MARGIN, FUN = .rank.numeric)
}



# Note that use of `which()` here will omit `NA` intentionally.

# nolint start
#
# x
# > gene1 gene2 gene3 gene4 gene5 gene6 gene7 gene8
# >  2.00  0.25 -1.25 -2.00  0.50  0.25  4.00  0.00
#
# .bidirRank.numeric(x, removeZeros = TRUE)
# > gene1 gene2 gene3 gene4 gene5 gene6 gene7 gene8
# >   4.0   1.5  -1.0  -2.0   3.0   1.5   5.0    NA
#
# nolint end

.bidirRank.numeric <- function(x, removeZeros = TRUE) {
    assert(
        is.numeric(x),
        isFlag(removeZeros)
    )
    ties <- "average"

    # Set any zero values to NA.
    if (isTRUE(removeZeros)) {
        x[x == 0L] <- NA
    }

    up <- rank(x = x[which(x > 0L)], ties.method = ties)
    down <- -rank(x = -x[which(x < 0L)], ties.method = ties)

    y <- x
    y[names(up)] <- up
    y[names(down)] <- down
    y
}



# nolint start
#
# x
# >       contrast1 contrast2 contrast3 contrast4
# > gene1     -2.00      2.00      0.50      4.00
# > gene2     -1.25      0.25      4.00      0.50
# > gene3      0.00     -1.25     -1.25     -2.00
# > gene4      0.25     -2.00      0.25      2.00
# > gene5      0.25      0.50      0.00      0.25
# > gene6      0.50      0.25      0.25     -1.25
# > gene7      2.00      4.00      2.00      0.25
# > gene8      4.00      0.00     -2.00      0.00
#
# .bidirRank.matrix(x, MARGIN = 2L, removeZeroes = TRUE)
# >       contrast1 contrast2 contrast3 contrast4
# > gene1      -2.0       4.0       3.0       5.0
# > gene2      -1.0       1.5       5.0       3.0
# > gene3        NA      -1.0      -1.0      -2.0
# > gene4       1.5      -2.0       1.5       4.0
# > gene5       1.5       3.0        NA       1.5
# > gene6       3.0       1.5       1.5      -1.0
# > gene7       4.0       5.0       4.0       1.5
# > gene8       5.0        NA      -2.0        NA
#
# nolint end

.bidirRank.matrix <- function(
    x,
    MARGIN = 2L,  # nolint
    removeZeros = TRUE
) {
    assert(
        is.matrix(x),
        isInt(MARGIN),
        isFlag(removeZeros)
    )
    apply(
        X = x,
        MARGIN = MARGIN,
        FUN = .bidirRank.numeric,
        removeZeros = removeZeros
    )
}
