#' Calculate a ranked matrix
#'
#' @note Ties are resolved automatically by calculating the average. See the
#'   `ties.method` parameter in [`rank()`][base::rank] for details.
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
#' data(lfc, package = "acidtest")
#'
#' ## Increasing (negative to positive)
#' rankedMatrix(lfc, method = "increasing")
#'
#' ## Decreasing (positive to negative)
#' rankedMatrix(lfc, method = "decreasing")
#'
#' ## Bidirectional
#' rankedMatrix(lfc, method = "bidirectional")
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
            removeZeros = FALSE
        )
    }

    mat
}



## Consider using `data.table::frank()` instead of `base::rank()` for speed.
## This adds an additional dependency, so avoid at the moment.
.rank.numeric <-  # nolint
    function(x, decreasing = FALSE) {
        assert(
            is.numeric(x),
            isFlag(decreasing),
            isFlag(decreasing)
        )
        r <- x
        if (isTRUE(decreasing)) r <- -r
        rank(r, na.last = TRUE, ties.method = "average")
    }



.rank.matrix <-  # nolint
    function(
        x,
        MARGIN = 2L,  # nolint
        decreasing = FALSE
    ) {
        assert(
            is.matrix(x),
            isFlag(decreasing),
            isInt(MARGIN)
        )
        apply(
            X = x,
            MARGIN = MARGIN,
            FUN = .rank.numeric,
            decreasing = decreasing
        )
    }



## Note that use of `which()` here will omit `NA` intentionally.
.bidirRank.numeric <-  # nolint
    function(x, removeZeros = FALSE) {
        assert(
            is.numeric(x),
            isFlag(removeZeros)
        )
        ties <- "average"

        ## Set any zero values to NA.
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



.bidirRank.matrix <-  # nolint
    function(
        x,
        MARGIN = 2L,  # nolint
        removeZeros = FALSE
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
