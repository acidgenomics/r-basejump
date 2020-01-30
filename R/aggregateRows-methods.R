#' @rdname aggregate
#' @name aggregateRows
#' @importFrom acidgenerics aggregateRows
#' @usage aggregateRows(object, ...)
#' @export
NULL



## Updated 2019-08-18.
`aggregateRows,matrix` <-  # nolint
    function(object, groupings, fun) {
        assert(
            hasValidDimnames(object),
            is.factor(groupings),
            identical(rownames(object), names(groupings)),
            validNames(levels(groupings))
        )
        fun <- match.arg(fun)
        .aggregateMessage(groupings, fun = fun)
        ## Using `stats::aggregate.data.frame()` S3 method here.
        data <- aggregate(
            x = object,
            by = list(rowname = groupings),
            FUN = getFromNamespace(x = fun, ns = "base")
        )
        assert(is.data.frame(data))
        rownames(data) <- data[["rowname"]]
        data[["rowname"]] <- NULL
        as.matrix(data)
    }

formals(`aggregateRows,matrix`)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("matrix"),
    definition = `aggregateRows,matrix`
)



## Updated 2019-07-22.
`aggregateRows,sparseMatrix` <-  # nolint
    function(object, groupings, fun) {
        validObject(object)
        assert(
            hasValidDimnames(object),
            is.factor(groupings),
            identical(rownames(object), names(groupings)),
            validNames(levels(groupings))
        )
        fun <- match.arg(fun)
        .aggregateMessage(groupings, fun = fun)
        ## FIXME `Matrix.utils::aggregate.Matrix` S3 method.
        aggregate(x = object, groupings = groupings, fun = fun)
    }

formals(`aggregateRows,sparseMatrix`)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("sparseMatrix"),
    definition = `aggregateRows,sparseMatrix`
)



## Updated 2019-07-22.
`aggregateRows,SummarizedExperiment` <-  # nolint
    function(object, col = "aggregate", fun) {
        validObject(object)
        assert(
            hasValidDimnames(object),
            isString(col)
        )
        fun <- match.arg(fun)

        ## Groupings -----------------------------------------------------------
        assert(isSubset(col, colnames(rowData(object))))
        groupings <- rowData(object)[[col]]
        assert(
            is.factor(groupings),
            validNames(levels(groupings))
        )
        names(groupings) <- rownames(object)

        ## Counts --------------------------------------------------------------
        counts <- aggregateRows(
            object = counts(object),
            groupings = groupings,
            fun = fun
        )
        if (fun == "sum") {
            assert(identical(x = sum(counts), y = sum(counts(object))))
        }

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

formals(`aggregateRows,SummarizedExperiment`)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("SummarizedExperiment"),
    definition = `aggregateRows,SummarizedExperiment`
)
