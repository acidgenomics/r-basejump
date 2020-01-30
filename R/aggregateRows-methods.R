## FIXME Rename generic to x.



#' @rdname aggregate
#' @name aggregateRows
#' @importFrom acidgenerics aggregateRows
#' @usage aggregateRows(object, ...)
#' @export
NULL



## Updated 2020-01-30.
`aggregateRows,matrix` <-  # nolint
    function(object, by, FUN) {
        assert(
            hasValidDimnames(object),
            is.factor(by),
            identical(rownames(object), names(by)),
            validNames(levels(by))
        )
        FUN <- match.fun(FUN)
        ## Using the `stats::aggregate.data.frame()` S3 method here.
        data <- aggregate(
            x = object,
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
    function(object, by, FUN) {
        validObject(object)
        assert(
            hasValidDimnames(object),
            is.factor(by),
            identical(rownames(object), names(by)),
            validNames(levels(by))
        )
        FUN <- match.fun(FUN)
        ## Using our internal Matrix S4 method here.
        aggregate(x = object, by = by, FUN = FUN)
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
        object,
        col = "aggregate",
        FUN
    ) {
        validObject(object)
        assert(
            hasValidDimnames(object),
            isString(col)
        )
        FUN <- match.fun(FUN)

        ## Groupings -----------------------------------------------------------
        assert(isSubset(col, colnames(rowData(object))))
        by <- rowData(object)[[col]]
        assert(
            is.factor(by),
            validNames(levels(by))
        )
        names(by) <- rownames(object)

        ## Counts --------------------------------------------------------------
        counts <- aggregateRows(object = counts(object), by = by, FUN = FUN)

        ## Return --------------------------------------------------------------
        args <- list(
            assays = SimpleList(counts = counts),
            colData = colData(object)
        )
        if (is(object, "RangedSummarizedExperiment")) {
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
