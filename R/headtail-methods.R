#' @name headtail
#' @inherit bioverbs::headtail
#' @inheritParams params
#'
#' @param n `integer(1)`.
#'   Positive integer denoting the number of first and last items to include.
#' @param ascii `logical(1)`.
#'   Require separators to use ASCII instead of Unicode.
#'
#' @examples
#' data(rse, package = "acidtest")
#'
#' ## data.frame ====
#' headtail(datasets::mtcars, ascii = TRUE)
#' headtail(dplyr::starwars)
#'
#' ## SummarizedExperiment ====
#' headtail(rse, ascii = TRUE)
NULL



#' @importFrom bioverbs headtail
#' @aliases NULL
#' @export
bioverbs::headtail



headtail.atomic <-  # nolint
    function(x, n = 2L) {
        assert(
            is.atomic(x),
            isInt(n),
            isPositive(n)
        )
        if (length(x) <= n * 2L) {
            out <- paste(x, collapse = " ")
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
        cat(out, sep = "\n")
        invisible()
    }



#' @describeIn headtail Paste collapse to a `character(1)`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("atomic"),
    definition = headtail.atomic
)



headtail.matrix <-  # nolint
    function(x, n = 2L, ascii = FALSE) {
        assert(
            hasDims(x),
            isInt(n),
            isPositive(n),
            isFlag(ascii)
        )

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
            assert(
                nrow(square) == n * 2L,
                ncol(square) == n * 2L
            )

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

        # Substitute Unicode characters for ASCII, if desired.
        if (isTRUE(ascii)) {
            dimnames(out) <- lapply(
                X = dimnames(out),
                FUN = iconv,
                from = "UTF-8",
                to = "ASCII",
                sub = "."
            )
            out <- as.data.frame(apply(
                X = out,
                MARGIN = c(1L, 2L),  # rows and columns
                FUN = iconv,
                from = "UTF-8",
                to = "ASCII",
                sub = "."
            ))
        }

        print(out)
        invisible()
    }



#' @describeIn headtail Show first and last rows.
#' @export
setMethod(
    f = "headtail",
    signature = signature("matrix"),
    definition = headtail.matrix
)



#' @describeIn headtail Same method as `matrix`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("sparseMatrix"),
    definition = headtail.matrix
)



#' @describeIn headtail Same method as `matrix`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("data.frame"),
    definition = headtail.matrix
)



#' @describeIn headtail Same method as `data.frame`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("DataFrame"),
    definition = headtail.matrix
)



headtail.GRanges <-  # nolint
    function() {
        headtail(
            x = as(x, "data.frame"),
            n = n,
            ascii = ascii
        )
    }

formals(headtail.GRanges) <- formals(headtail.matrix)



#' @describeIn headtail Summarize the ranges.
#' @export
setMethod(
    f = "headtail",
    signature = signature("GRanges"),
    definition = headtail.GRanges
)



headtail.SummarizedExperiment <-  # nolint
    function() {
        headtail(
            x = assay(x),
            n = n,
            ascii = ascii
        )
    }

formals(headtail.SummarizedExperiment) <- formals(headtail.matrix)



#' @describeIn headtail Summarize the primary `assay`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("SummarizedExperiment"),
    definition = headtail.SummarizedExperiment
)
