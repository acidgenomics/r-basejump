#' @name aggregateRows
#' @inherit acidgenerics::aggregateRows
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
