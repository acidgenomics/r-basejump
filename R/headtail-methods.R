#' Return the First and Last Part of an Object
#'
#' Inspired by the [print()] method for `DataFrame` class objects. Applies to
#' both rows and columns, enabling quick inspection during interactive use.
#'
#' @name headtail
#' @export
#'
#' @inheritParams general
#' @param n `scalar integer`. Positive integer denoting the number of first and
#'   last items to include.
#'
#' @return None (invisible `NULL`).
#'
#' @seealso
#' - [head()], [tail()], [cat()].
#' - `getMethod("show", "DataTable")`.
#' - [S4Vectors::showAsCell].
#'
#' @examples
#' data(rse_small, sce_small)
#'
#' ## data.frame ====
#' headtail(datasets::mtcars)
#' headtail(dplyr::starwars)
#'
#' ## SummarizedExperiment ====
#' headtail(rse_small)
#'
#' ## SingleCellExperiment ====
#' headtail(sce_small)
NULL



.headtail.atomic <-  # nolint
    function(x, n = 2L) {
        assert_is_atomic(x)
        assertIsAnImplicitInteger(n)
        assert_all_are_positive(n)
        if (length(x) <= n) {
            message("Returning entire vector.")
            out <- x
        } else {
            out <- paste(
                c(
                    head(x, n = n),
                    "...",
                    tail(x, n = n)
                ),
                collapse = " "
            )
        }
        cat(out)
        invisible()
    }



.headtail.data.frame <-  # nolint
    function(x, n = 2L) {
        assert_has_dims(x)
        assertIsAnImplicitInteger(n)
        assert_all_are_positive(n)

        if (nrow(x) <= n * 2L || ncol(x) <= n * 2L) {
            message("Object can't be split into quadrants.")
            out <- x[
                head(rownames(x), n = n * 2L),
                head(colnames(x), n = n * 2L),
                drop = FALSE
            ]
            out <- as.data.frame(out)
        } else {
            # Ensure that we're performing subset operation before coercion to
            # data.frame, as this can blow up in memory for sparse matrices.
            square <- x[
                c(
                    head(rownames(x), n = n),
                    tail(rownames(x), n = n)
                ),
                c(
                    head(colnames(x), n = n),
                    tail(colnames(x), n = n)
                ),
                drop = FALSE
            ]

            # Coerce to data.frame, for consistency.
            square <- as.data.frame(square)

            # Sanitize all non-atomic columns to placeholder symbol.
            list <- lapply(
                X = square,
                FUN = function(x) {
                    if (is.factor(x)) {
                        as.character(x)
                    } else if (is.atomic(x)) {
                        x
                    } else {
                        "<>"
                    }
                }
            )
            # Now safe to coerce to atomic data.frame.
            square <- data.frame(
                list,
                row.names = rownames(square),
                check.rows = FALSE,
                check.names = FALSE,
                stringsAsFactors = FALSE
            )

            # Check that we have square dimensions.
            stopifnot(nrow(square) == n * 2L)
            stopifnot(ncol(square) == n * 2L)

            # Split into quadrants, so we can add vertical separators.
            # upper/lower, left/right.
            ul <- square[seq_len(n), seq_len(n), drop = FALSE]
            ur <- square[seq_len(n), seq_len(n) + n, drop = FALSE]
            ll <- square[seq_len(n) + n, seq_len(n), drop = FALSE]
            lr <- square[seq_len(n) + n, seq_len(n) + n, drop = FALSE]

            head <- data.frame(
                ul,
                "\u2502" = rep("\u2502", times = n),
                ur,
                check.rows = FALSE,
                check.names = FALSE,
                stringsAsFactors = FALSE
            )
            tail <- data.frame(
                ll,
                "\u2502" = rep("\u2502", times = n),
                lr,
                check.rows = FALSE,
                check.names = FALSE,
                stringsAsFactors = FALSE
            )
            out <- rbind(
                head,
                "\u2500" = c(
                    rep("\u2500", times = n),
                    "\u253C",
                    rep("\u2500", times = n)
                ),
                tail,
                stringsAsFactors = FALSE
            )
        }

        print(out)
        invisible()
    }



#' @describeIn headtail Paste collapse to a `string`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("atomic"),
    definition = .headtail.atomic
)



#' @describeIn headtail Show first and last rows.
#' @export
setMethod(
    f = "headtail",
    signature = signature("matrix"),
    definition = .headtail.data.frame
)


#' @describeIn headtail Same method as `matrix`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("sparseMatrix"),
    definition = getMethod("headtail", "matrix")
)



#' @describeIn headtail Same method as `matrix`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("data.frame"),
    definition = getMethod("headtail", "matrix")
)



#' @describeIn headtail Same method as `data.frame`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("DataFrame"),
    definition = getMethod("headtail", "data.frame")
)



#' @describeIn headtail Summarize the ranges.
#' @export
setMethod(
    f = "headtail",
    signature = signature("GRanges"),
    definition = function(x, n = 2L) {
        headtail(as(x, "data.frame"), n = n)
    }
)



#' @describeIn headtail Summarize the primary [assay()].
#' @export
setMethod(
    f = "headtail",
    signature = signature("SummarizedExperiment"),
    definition = function(x, n = 2L) {
        headtail(assay(x), n = n)
    }
)
