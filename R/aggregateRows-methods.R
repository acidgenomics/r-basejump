#' @rdname aggregate
#' @name aggregateRows
#' @importFrom acidgenerics aggregateRows
#' @usage aggregateRows(x, ...)
#' @export
NULL



## Updated 2020-01-30.
`aggregateRows,matrix` <-  # nolint
    function(x, by, FUN) {
        assert(
            hasValidDimnames(x),
            is.factor(by),
            identical(rownames(x), names(by)),
            validNames(levels(by))
        )
        FUN <- match.fun(FUN)
        ## Using the `stats::aggregate.data.frame()` S3 method here.
        data <- aggregate(
            x = x,
            by = list(rowname = by),
            FUN = FUN
        )
        assert(is.data.frame(data))
        rownames(data) <- data[["rowname"]]
        data[["rowname"]] <- NULL
        as.matrix(data)
    }



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("matrix"),
    definition = `aggregateRows,matrix`
)



## Updated 2020-01-30.
`aggregateRows,sparseMatrix` <-  # nolint
    function(x, by, FUN) {
        validObject(x)
        assert(
            hasValidDimnames(x),
            is.factor(by),
            identical(rownames(x), names(by)),
            validNames(levels(by))
        )
        FUN <- match.fun(FUN)
        ## Using our internal Matrix S4 method here.
        aggregate(x = x, by = by, FUN = FUN)
    }



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("sparseMatrix"),
    definition = `aggregateRows,sparseMatrix`
)



## Updated 2020-01-30.
`aggregateRows,SummarizedExperiment` <-  # nolint
    function(
        x,
        col = "aggregate",
        FUN
    ) {
        validObject(x)
        assert(
            hasValidDimnames(x),
            isString(col)
        )
        FUN <- match.fun(FUN)

        ## Groupings -----------------------------------------------------------
        assert(isSubset(col, colnames(rowData(x))))
        by <- rowData(x)[[col]]
        assert(
            is.factor(by),
            validNames(levels(by))
        )
        names(by) <- rownames(x)

        ## Counts --------------------------------------------------------------
        counts <- aggregateRows(x = counts(x), by = by, FUN = FUN)

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



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("SummarizedExperiment"),
    definition = `aggregateRows,SummarizedExperiment`
)
