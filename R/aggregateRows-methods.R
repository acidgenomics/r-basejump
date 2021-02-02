#' @name aggregateRows
#' @inherit AcidGenerics::aggregateRows
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @section Methods (by class):
#'
#' - `matrix`, `Matrix`:
#'   Aggregate using a grouping `factor`.
#' - `SummarizedExperiment`:
#'   Aggregate data slotted in
#'   [`assays()`][SummarizedExperiment::assays] using an automatically
#'   generated grouping `factor`, which is obtained from a user-defined column
#'   (`col` argument) in either the
#'   [`rowData()`][SummarizedExperiment::rowData] or
#'   [`colData()`][SummarizedExperiment::colData] of the object. Slot an
#'   `aggregate` column into [`rowData()`][SummarizedExperiment::rowData]
#'   for [aggregateRows()], or into
#'   [`colData()`][SummarizedExperiment::colData] for [aggregateCols()]. This
#'   method will define the `groupings` automatically, and perform the
#'   aggregation.
#'
#' @inheritParams aggregate
#' @param col `character(1)`.
#'   Name of column in either [`rowData()`][SummarizedExperiment::rowData] or
#'   [`colData()`][SummarizedExperiment::colData] that defines the desired
#'   aggregation groupings.
#' @param ... Additional arguments.
#'
#' @examples
#' counts <- matrix(
#'     data = c(
#'         0L, 2L, 2L, 2L,
#'         2L, 0L, 2L, 2L,
#'         2L, 2L, 0L, 2L,
#'         2L, 2L, 2L, 0L
#'     ),
#'     nrow = 4L,
#'     ncol = 4L,
#'     byrow = TRUE,
#'     dimnames = list(
#'         paste0("transcript", seq_len(4L)),
#'         paste(
#'             paste0("sample", rep(seq_len(2L), each = 2L)),
#'             paste0("replicate", rep(seq_len(2L), times = 2L)),
#'             sep = "_"
#'         )
#'     )
#' )
#'
#' genes <- factor(paste0("gene", rep(seq_len(2L), each = 2L)))
#' names(genes) <- rownames(counts)
#' print(genes)
#'
#' ## matrix ====
#' print(counts)
#' aggregateRows(counts, by = genes)
#'
#' ## Matrix ====
#' sparse <- as(counts, "sparseMatrix")
#' print(sparse)
#' aggregateRows(sparse, by = genes)
#'
#' ## SummarizedExperiment ====
#' se <- SummarizedExperiment::SummarizedExperiment(
#'     assays = SimpleList(counts = counts),
#'     rowData = DataFrame(aggregate = genes)
#' )
#' print(se)
#' aggregateRows(se)
NULL



## Updated 2020-01-30.
`aggregateRows,matrix` <-  # nolint
    `aggregate,matrix`



#' @rdname aggregateRows
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("matrix"),
    definition = `aggregateRows,matrix`
)



## Updated 2020-05-22.
`aggregateRows,Matrix` <-  # nolint
    function(
        x,
        by,
        fun = c("sum", "mean")
    ) {
        validObject(x)
        assert(
            hasDimnames(x),
            is.factor(by),
            identical(rownames(x), names(by))
        )
        fun <- match.arg(fun)
        ## Using our internal Matrix S4 method here.
        aggregate(x = x, by = by, fun = fun)
    }



#' @rdname aggregateRows
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("Matrix"),
    definition = `aggregateRows,Matrix`
)



## Updated 2020-05-22.
`aggregateRows,SummarizedExperiment` <-  # nolint
    function(
        x,
        col = "aggregate",
        fun = "sum"
    ) {
        validObject(x)
        assert(
            hasDimnames(x),
            isString(col),
            isString(fun)
        )
        ## Groupings -----------------------------------------------------------
        assert(isSubset(col, colnames(rowData(x))))
        by <- rowData(x)[[col]]
        assert(is.factor(by))
        names(by) <- rownames(x)
        ## Counts --------------------------------------------------------------
        counts <- aggregateRows(x = counts(x), by = by, fun = fun)
        ## Return --------------------------------------------------------------
        args <- list(
            assays = SimpleList(counts = counts),
            colData = colData(x)
        )
        if (is(x, "RangedSummarizedExperiment")) {
            args[["rowRanges"]] <- emptyRanges(names = rownames(counts))
        } else {
            args[["rowData"]] <- DataFrame(row.names = rownames(counts))
        }
        se <- do.call(what = SummarizedExperiment, args = args)
        metadata(se)[["aggregate"]] <- TRUE
        validObject(se)
        se
    }



#' @rdname aggregateRows
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("SummarizedExperiment"),
    definition = `aggregateRows,SummarizedExperiment`
)
