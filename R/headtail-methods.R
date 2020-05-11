#' @name headtail
#' @inherit acidgenerics::headtail
#' @note Updated 2020-05-11.
#'
#' @inheritParams acidroxygen::params
#' @param n `integer(1)`.
#'   Positive integer denoting the number of first and last items to include.
#' @param ... Additional arguments.
#'
#' @examples
#' data(mtcars, package = "datasets")
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#'
#' ## data.frame ====
#' headtail(mtcars)
#'
#' ## SummarizedExperiment ====
#' headtail(rse)
NULL



#' @rdname headtail
#' @name headtail
#' @importFrom acidgenerics headtail
#' @usage headtail(x, ...)
#' @export
NULL



## Updated 2019-07-22.
`headtail,atomic` <-  # nolint
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
    definition = `headtail,atomic`
)



## Updated 2020-05-11.
`headtail,matrix` <-  # nolint
    function(x, n = 2L) {
        assert(
            hasDims(x),
            isInt(n),
            isPositive(n)
        )
        if (nrow(x) <= n * 2L || ncol(x) <= n * 2L) {
            cli_alert_warning("Object can't be split into quadrants.")
            out <- x[
                head(rownames(x), n = n * 2L),
                head(colnames(x), n = n * 2L),
                drop = FALSE
                ]
            out <- as.data.frame(out)
        } else {
            ## Ensure that we're performing subset operation before coercion to
            ## data.frame, as this can blow up in memory for sparse matrices.
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
            ## Coerce to data.frame, for consistency.
            square <- as.data.frame(square)
            ## Sanitize all non-atomic columns to placeholder symbol.
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
            ## Now safe to coerce to atomic data.frame.
            square <- data.frame(
                list,
                row.names = rownames(square),
                check.rows = FALSE,
                check.names = FALSE,
                stringsAsFactors = FALSE
            )
            ## Check that we have square dimensions.
            assert(
                nrow(square) == n * 2L,
                ncol(square) == n * 2L
            )
            ## Split into quadrants, so we can add vertical separators.
            ## upper/lower, left/right.
            ul <- square[seq_len(n), seq_len(n), drop = FALSE]
            ur <- square[seq_len(n), seq_len(n) + n, drop = FALSE]
            ll <- square[seq_len(n) + n, seq_len(n), drop = FALSE]
            lr <- square[seq_len(n) + n, seq_len(n) + n, drop = FALSE]
            head <- data.frame(
                ul,
                "..." = rep("...", times = n),  # \u2502
                ur,
                check.rows = FALSE,
                check.names = FALSE,
                stringsAsFactors = FALSE
            )
            tail <- data.frame(
                ll,
                "..." = rep("...", times = n),  # \u2502
                lr,
                check.rows = FALSE,
                check.names = FALSE,
                stringsAsFactors = FALSE
            )
            out <- rbind(
                head,
                "..." = c(  # "\u2500"
                    rep("...", times = n),  # \u2500
                    "...",  # \u253C
                    rep("...", times = n)  # \u2500
                ),
                tail,
                stringsAsFactors = FALSE
            )
        }
        print(out)
        invisible()
    }



#' @describeIn headtail Show first and last rows.
#' @export
setMethod(
    f = "headtail",
    signature = signature("matrix"),
    definition = `headtail,matrix`
)



## Updated 2020-01-30.
`headtail,Matrix` <-  # nolint
    `headtail,matrix`



#' @describeIn headtail Same method as `matrix`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("Matrix"),
    definition = `headtail,Matrix`
)



## Updated 2019-07-22.
`headtail,data.frame` <-  # nolint
    `headtail,matrix`



#' @describeIn headtail Same method as `matrix`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("data.frame"),
    definition = `headtail,data.frame`
)



## Updated 2019-07-22.
`headtail,DataFrame` <-  # nolint
    `headtail,data.frame`



#' @describeIn headtail Same method as `data.frame`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("DataFrame"),
    definition = `headtail,DataFrame`
)



## Updated 2020-05-11.
`headtail,GRanges` <-  # nolint
    function() {
        headtail(
            x = as(x, "data.frame"),
            n = n
        )
    }

formals(`headtail,GRanges`) <- formals(`headtail,matrix`)



#' @describeIn headtail Summarize the ranges.
#' @export
setMethod(
    f = "headtail",
    signature = signature("GRanges"),
    definition = `headtail,GRanges`
)



## Updated 2020-05-11.
`headtail,SummarizedExperiment` <-  # nolint
    function() {
        headtail(
            x = assay(x),
            n = n
        )
    }

formals(`headtail,SummarizedExperiment`) <- formals(`headtail,matrix`)



#' @describeIn headtail Summarize the primary `assay`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("SummarizedExperiment"),
    definition = `headtail,SummarizedExperiment`
)
