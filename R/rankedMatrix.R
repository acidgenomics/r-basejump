#' Calculate a ranked matrix
#'
#' @note Ties are resolved automatically by calculating the average. See the
#'   `ties.method` parameter in [`rank()]][base::rank] for details.
#'
#' @export
#' @inheritParams params
#' @inheritParams base::apply
#'
#' @param decreasing `logical(1)`.
#'   Rank the values from positive to negative.
#'
#' @seealso Not to be confused with [Matrix::rankMatrix()].
#'
#' @examples
#' ## Order was randomized using `sample()`.
#' lfc <- matrix(
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
    method = c("decreasing", "increasing", "bidirectional"),
    MARGIN = 2L
) {
    method <- match.arg(method)
    if (method %in% c("decreasing", "increasing")) {
        if (method == "decreasing") {
            decreasing <- TRUE
        } else if (method == "increasing") {
            decreasing <- FALSE
        }
    } else if (method == "directional") {
    }
    mat
}



# Consider using `data.table::frank()` here instead of `base::rank()` for speed.

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
    assert(is.numeric(x), isFlag(decreasing))
    r <- x
    if (isTRUE(decreasing)) r <- -r
    rank(r, na.last = TRUE, ties.method = "average")
}



# Note that use of `which()` here will omit `NA` intentionally.

# nolint start
#
# x
# > gene1 gene2 gene3 gene4 gene5 gene6 gene7 gene8
# >  2.00  0.25 -1.25 -2.00  0.50  0.25  4.00  0.00
#
# .bidirRank.numeric(x)
# > gene1 gene2 gene3 gene4 gene5 gene6 gene7 gene8
# >   4.0   1.5  -1.0  -2.0   3.0   1.5   5.0    NA
#
# nolint end

.bidirRank.numeric <- function(x) {
    ties <- "average"

    # First, set any zero values to NA.
    x[x == 0L] <- NA

    up <- rank(x = x[which(x > 0L)], ties.method = ties)
    down <- -rank(x = -x[which(x < 0L)], ties.method = ties)

    y <- x
    y[names(up)] <- up
    y[names(down)] <- down
    y
}
