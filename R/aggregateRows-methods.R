#' @name aggregateRows
#' @inherit acidgenerics::aggregateRows
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
#' - `SingleCellExperiment`:
#'   Aggregate [`assays()`][SummarizedExperiment::assays] across cell-level
#'   groupings, defined by a column in
#'   [`colData()`][SummarizedExperiment::colData]. Inherits from
#'   `SummarizedExperiment`, and still relies upon slotting an `aggregate`
#'   column into [`colData()`][SummarizedExperiment::colData]. Note that these
#'   groupings will map to cells, so care must be taken to properly aggregate
#'   samples.
#'
#' @param col `character(1)`.
#'   Name of column in either [`rowData()`][SummarizedExperiment::rowData] or
#'   [`colData()`][SummarizedExperiment::colData] that defines the desired
#'   aggregation groupings.
NULL



#' @rdname aggregateRows
#' @name aggregateRows
#' @importFrom acidgenerics aggregateRows
#' @usage aggregateRows(x, ...)
#' @export
NULL



## Updated 2020-01-30.
`aggregateRows,matrix` <-  # nolint
    function(
        x,
        by,
        fun = c("sum", "mean", "median", "geometricMean")
    ) {
        assert(
            hasValidDimnames(x),
            is.factor(by),
            identical(rownames(x), names(by)),
            validNames(levels(by))
        )
        fun <- match.arg(fun)
        ## Using the `stats::aggregate.data.frame()` S3 method here.
        data <- aggregate(
            x = x,
            by = list(rowname = by),
            FUN = get(x = fun, inherits = TRUE)
        )
        assert(is.data.frame(data))
        rownames(data) <- data[["rowname"]]
        data[["rowname"]] <- NULL
        as.matrix(data)
    }



#' @rdname aggregateRows
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("matrix"),
    definition = `aggregateRows,matrix`
)



## Updated 2020-01-30.
`aggregateRows,Matrix` <-  # nolint
    function(
        x,
        by,
        fun = c("sum", "mean")
    ) {
        validObject(x)
        assert(
            hasValidDimnames(x),
            is.factor(by),
            identical(rownames(x), names(by)),
            validNames(levels(by))
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



## Updated 2020-01-30.
`aggregateRows,SummarizedExperiment` <-  # nolint
    function(
        x,
        col = "aggregate",
        fun = "sum"
    ) {
        validObject(x)
        assert(
            hasValidDimnames(x),
            isString(col),
            isString(fun)
        )

        ## Groupings -----------------------------------------------------------
        assert(isSubset(col, colnames(rowData(x))))
        by <- rowData(x)[[col]]
        assert(
            is.factor(by),
            validNames(levels(by))
        )
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
