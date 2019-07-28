#' Aggregate rows or columns
#'
#' Aggregate gene/transcript features (rows) or sample replicates (columns).
#'
#' [aggregateRows()] works down the rows, and is designed to aggregate features
#' (e.g. genes or transcripts). Most commonly, the [aggregateRows()] function
#' can be used to aggregate counts from transcript-level to gene-level.
#' [aggregateCols()] works across the columns, and is designed to aggregate
#' sample replicates.
#'
#' @name aggregate
#' @author Michael Steinbaugh, Rory Kirchner
#' @note Updated 2019-07-28.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @section Methods (by class):
#'
#' - `matrix`, `sparseMatrix`:
#'     Aggregate rows or columns using a grouping `factor`.
#' - `SummarizedExperiment`:
#'     Aggregate rows or columns of data slotted in
#'     [`assays()`][SummarizedExperiment::assays] using an automatically
#'     generated grouping `factor`, which is obtained from a user-defined column
#'     (`col` argument) in either the
#'     [`rowData()`][SummarizedExperiment::rowData] or
#'     [`colData()`][SummarizedExperiment::colData] of the object. Slot an
#'     `aggregate` column into [`rowData()`][SummarizedExperiment::rowData]
#'     for [aggregateRows()], or into
#'     [`colData()`][SummarizedExperiment::colData] for [aggregateCols()]. This
#'     method will define the `groupings` automatically, and perform the
#'     aggregation.
#' - `SingleCellExperiment`:
#'     Aggregate [`assays()`][SummarizedExperiment::assays] across cell-level
#'     groupings, defined by a column in
#'     [`colData()`][SummarizedExperiment::colData]. Inherits from
#'     `SummarizedExperiment`, and still relies upon slotting an `aggregate`
#'     column into [`colData()`][SummarizedExperiment::colData]. Note that these
#'     groupings will map to cells, so care must be taken to properly aggregate
#'     samples.
#'
#' @param groupings `factor`.
#'   Defines the aggregation groupings. The new aggregate names are defined as
#'   the `factor` [levels][base::levels], and the original, unaggregated names
#'   are defined as the [names][base::names].
#' @param col `character(1)`.
#'   Name of column in either [`rowData()`][SummarizedExperiment::rowData] or
#'   [`colData()`][SummarizedExperiment::colData] that defines the desired
#'   aggregation groupings.
#' @param fun `character(1)`.
#'   Name of the aggregation function.
#'   Uses [`match.arg()`][base::match.arg] internally.
#'
#' @seealso
#' - `stats::aggregate()`.
#' - `S4Vectors::aggregate()`.
#' - `Matrix.utils::aggregate.Matrix()`.
#'
#' @return Modified object, with aggregated rows (features) or columns
#'   (samples).
#'
#' @examples
#' ## Example data ====
#' assay <- matrix(
#'     data = c(
#'         0L, 1L, 1L, 1L,
#'         1L, 0L, 1L, 1L,
#'         1L, 1L, 0L, 1L,
#'         1L, 1L, 1L, 0L
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
#' class(assay)
#' print(assay)
#'
#' genes <- factor(paste0("gene", rep(seq_len(2L), each = 2L)))
#' names(genes) <- rownames(assay)
#' print(genes)
#'
#' samples <- factor(paste0("sample", rep(seq_len(2L), each = 2L)))
#' names(samples) <- colnames(assay)
#' print(samples)
#'
#' ## sparseMatrix
#' sparse <- as(assay, "sparseMatrix")
#' class(sparse)
#' print(sparse)
#'
#' ## SummarizedExperiment
#' se <- SummarizedExperiment::SummarizedExperiment(
#'     assay = list(assay = assay),
#'     colData = S4Vectors::DataFrame(
#'         sampleName = as.factor(names(samples)),
#'         aggregate = samples
#'     ),
#'     rowData = S4Vectors::DataFrame(aggregate = genes)
#' )
#' print(se)
#'
#' ## aggregateRows ====
#' aggregateRows(assay, groupings = genes)
#' aggregateRows(sparse, groupings = genes)
#' aggregateRows(se)
#'
#' ## aggregateCols ====
#' aggregateCols(assay, groupings = samples)
#' aggregateCols(sparse, groupings = samples)
#' aggregateCols(se)
NULL



#' @rdname aggregate
#' @name aggregateCols
#' @importFrom bioverbs aggregateCols
#' @usage aggregateCols(object, ...)
#' @export
NULL

#' @rdname aggregate
#' @name aggregateRows
#' @importFrom bioverbs aggregateRows
#' @usage aggregateRows(object, ...)
#' @export
NULL



.aggregateFuns <- c("sum", "mean")

## Don't message when aggregating a large factor.
.aggregateMessage <- function(groupings, fun) {
    assert(
        is.factor(groupings),
        isString(fun)
    )
    msg <- paste0("Aggregating counts using ", fun, "().")
    if (length(groupings) <= 20L) {
        msg <- paste(
            msg,
            "Groupings:",
            printString(groupings),
            sep = "\n"
        )
    }
    message(msg)
}



## aggregateRows ===============================================================
## Updated 2019-07-22.
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
        data <- column_to_rownames(data, var = "rowname")
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
        ## `Matrix.utils::aggregate.Matrix` S3 method.
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

        ## Assay ---------------------------------------------------------------
        assay <- aggregateRows(
            object = assay(object),
            groupings = groupings,
            fun = fun
        )
        if (fun == "sum") {
            assert(identical(sum(assay), sum(assay(object))))
        }

        ## Return --------------------------------------------------------------
        args <- list(
            assays = list(assay),
            colData = colData(object)
        )
        if (is(object, "RangedSummarizedExperiment")) {
            args[["rowRanges"]] <- emptyRanges(names = rownames(assay))
        } else {
            args[["rowData"]] <- DataFrame(row.names = rownames(assay))
        }
        se <- do.call(what = SummarizedExperiment, args = args)
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



## aggregateCols ===============================================================
## Updated 2019-07-22.
`aggregateCols,matrix` <-  # nolint
    function(
        object,
        groupings,
        fun
    ) {
        fun <- match.arg(fun)
        object <- t(object)
        object <- aggregateRows(
            object = object,
            groupings = groupings,
            fun = fun
        )
        assert(is.matrix(object))
        object <- t(object)
        object
    }

formals(`aggregateCols,matrix`)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("matrix"),
    definition = `aggregateCols,matrix`
)



## Updated 2019-07-22.
`aggregateCols,sparseMatrix` <-  # nolint
    function(
        object,
        groupings,
        fun
    ) {
        fun <- match.arg(fun)
        object <- Matrix::t(object)
        object <- aggregateRows(
            object = object,
            groupings = groupings,
            fun = fun
        )
        assert(is(object, "Matrix"))
        object <- Matrix::t(object)
        object
    }

formals(`aggregateCols,sparseMatrix`)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("sparseMatrix"),
    definition = `aggregateCols,sparseMatrix`
)



## Updated 2019-07-22.
`aggregateCols,SummarizedExperiment` <-  # nolint
    function(object, col = "aggregate", fun) {
        validObject(object)
        assert(
            hasValidDimnames(object),
            isString(col)
        )
        fun <- match.arg(fun)

        ## Groupings -----------------------------------------------------------
        if (!all(
            isSubset(col, colnames(colData(object))),
            isSubset(col, colnames(sampleData(object)))
        )) {
            stop(paste(
                deparse(col), "column not defined in colData()."
            ))
        }
        groupings <- colData(object)[[col]]
        assert(
            is.factor(groupings),
            validNames(levels(groupings)),
            identical(length(groupings), ncol(object))
        )
        names(groupings) <- colnames(object)

        ## Assay ---------------------------------------------------------------
        assay <- aggregateCols(
            object = assay(object),
            groupings = groupings,
            fun = fun
        )
        assert(identical(nrow(assay), nrow(object)))
        if (fun == "sum") {
            assert(identical(sum(assay), sum(assay(object))))
        }

        ## Return --------------------------------------------------------------
        args <- list(
            assays = list(assay),
            colData = DataFrame(row.names = colnames(assay))
        )
        if (is(object, "RangedSummarizedExperiment")) {
            args[["rowRanges"]] <- rowRanges(object)
        } else {
            args[["rowData"]] <- rowData(object)
        }
        se <- do.call(what = SummarizedExperiment, args = args)
        metadata(se)[["aggregate"]] <- TRUE
        validObject(se)
        se
    }

formals(`aggregateCols,SummarizedExperiment`)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SummarizedExperiment"),
    definition = `aggregateCols,SummarizedExperiment`
)



## Updated 2019-07-22.
`aggregateCols,SingleCellExperiment` <-  # nolint
    function(object, fun) {
        validObject(object)
        fun <- match.arg(fun)

        ## Remap cellular barcode groupings ------------------------------------
        colData <- colData(object)
        assert(
            isSubset(c("sampleID", "aggregate"), colnames(colData)),
            is.factor(colData[["aggregate"]])
        )

        message(paste(
            "Remapping cells to aggregate samples:",
            toString(sort(levels(colData[["aggregate"]])))
        ))

        map <- colData(object)
        map <- as_tibble(map, rownames = "cellID")
        map <- map[, c("cellID", "sampleID", "aggregate")]

        ## Check to see if we can aggregate.
        if (!all(mapply(
            FUN = grepl,
            x = map[["cellID"]],
            pattern = paste0("^", map[["sampleID"]]),
            SIMPLIFY = TRUE
        ))) {
            stop("Cell IDs are not prefixed with sample IDs.")
        }
        groupings <- mapply(
            FUN = gsub,
            x = map[["cellID"]],
            pattern = paste0("^", map[["sampleID"]]),
            replacement = map[["aggregate"]],
            SIMPLIFY = TRUE,
            USE.NAMES = TRUE
        )
        groupings <- as.factor(groupings)

        cell2sample <- as.factor(map[["aggregate"]])
        names(cell2sample) <- as.character(groupings)

        ## Reslot the `aggregate` column using these groupings.
        assert(identical(names(groupings), colnames(object)))
        colData(object)[["aggregate"]] <- groupings

        ## Generate SingleCellExperiment ---------------------------------------
        ## Using `SummarizedExperiment` method here.
        rse <- as(object, "RangedSummarizedExperiment")
        colData(rse)[["sampleID"]] <- NULL
        rse <- aggregateCols(object = rse, fun = fun)
        assert(
            is(rse, "RangedSummarizedExperiment"),
            identical(nrow(rse), nrow(object))
        )

        ## Update the sample data.
        colData <- colData(rse)
        assert(isSubset(rownames(colData), names(cell2sample)))
        colData[["sampleID"]] <- cell2sample[rownames(colData)]
        colData[["sampleName"]] <- colData[["sampleID"]]
        colData(rse) <- colData

        ## Now ready to generate aggregated SCE.
        sce <- makeSingleCellExperiment(
            assays = list(counts = assay(rse)),
            rowRanges = rowRanges(object),
            colData = colData(rse),
            metadata = list(
                aggregate = TRUE,
                aggregateCols = groupings,
                interestingGroups = interestingGroups(object)
            ),
            spikeNames = spikeNames(object)
        )
        validObject(sce)
        sce
    }

formals(`aggregateCols,SingleCellExperiment`)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SingleCellExperiment"),
    definition = `aggregateCols,SingleCellExperiment`
)
