#' Aggregate rows or columns
#'
#' Aggregate gene/transcript features (rows) or sample replicates (columns).
#'
#' [aggregateRows()] works down the rows, and is designed to aggregate features
#' (e.g. genes or transcripts). Most commonly, the [aggregateRows()] function
#' can be used to aggregate counts from transcript-level to gene-level.
#'
#' [aggregateCols()] works across the columns, and is designed to aggregate
#' sample replicates.
#'
#' @name aggregate
#' @author Michael Steinbaugh, Rory Kirchner
#' @inheritParams params
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
#' counts <- matrix(
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
#' print(counts)
#'
#' genes <- factor(paste0("gene", rep(seq_len(2L), each = 2L)))
#' names(genes) <- rownames(counts)
#' print(genes)
#'
#' samples <- factor(paste0("sample", rep(seq_len(2L), each = 2L)))
#' names(samples) <- colnames(counts)
#' print(samples)
#'
#' cells <- factor(paste0("cell", rep(seq_len(2L), each = 2L)))
#' names(samples) <- colnames(counts)
#' print(samples)
#'
#' ## matrix
#' matrix <- as(counts, "matrix")
#' class(matrix)
#' print(matrix)
#'
#' ## sparseMatrix
#' sparse <- as(matrix, "sparseMatrix")
#' class(sparse)
#' print(sparse)
#'
#' ## SummarizedExperiment
#' se <- SummarizedExperiment::SummarizedExperiment(
#'     assay = list(counts = sparse),
#'     colData = S4Vectors::DataFrame(
#'         sampleName = as.factor(names(samples)),
#'         aggregate = samples
#'     ),
#'     rowData = S4Vectors::DataFrame(aggregate = genes)
#' )
#' print(se)
#'
#' ## aggregateRows ====
#' aggregateRows(matrix, groupings = genes)
#' aggregateRows(sparse, groupings = genes)
#' aggregateRows(se)
#'
#' ## aggregateCols ====
#' aggregateCols(matrix, groupings = samples)
#' aggregateCols(sparse, groupings = samples)
#' aggregateCols(se)
NULL



#' @rdname aggregate
#' @name aggregateRows
#' @importFrom bioverbs aggregateRows
#' @export
NULL

#' @rdname aggregate
#' @name aggregateCols
#' @importFrom bioverbs aggregateCols
#' @export
NULL



.aggregateFuns <- c("sum", "mean")

# Don't message when aggregating a large factor.
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



# aggregateRows ================================================================
aggregateRows.matrix <-  # nolint
    function(object, groupings, fun) {
        assert(
            hasValidDimnames(object),
            is.factor(groupings),
            identical(rownames(object), names(groupings)),
            validNames(levels(groupings))
        )

        fun <- match.arg(fun)
        .aggregateMessage(groupings, fun = fun)
        # `stats::aggregate.data.frame` S3 method.
        aggregate(
            x = object,
            by = list(rowname = groupings),
            FUN = getFromNamespace(x = fun, ns = "base")
        ) %>%
            column_to_rownames() %>%
            as.matrix()
    }

formals(aggregateRows.matrix)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("matrix"),
    definition = aggregateRows.matrix
)



aggregateRows.sparseMatrix <-  # nolint
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
        # `Matrix.utils::aggregate.Matrix` S3 method.
        aggregate(x = object, groupings = groupings, fun = fun)
    }

formals(aggregateRows.sparseMatrix)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("sparseMatrix"),
    definition = aggregateRows.sparseMatrix
)



aggregateRows.SummarizedExperiment <-  # nolint
    function(object, col = "aggregate", fun) {
        validObject(object)
        assert(
            hasValidDimnames(object),
            isString(col)
        )
        fun <- match.arg(fun)

        # Groupings ------------------------------------------------------------
        assert(isSubset(col, colnames(rowData(object))))
        groupings <- rowData(object)[[col]]
        assert(
            is.factor(groupings),
            validNames(levels(groupings))
        )
        names(groupings) <- rownames(object)

        # Assays ---------------------------------------------------------------
        counts <- aggregateRows(
            object = counts(object),
            groupings = groupings,
            fun = fun
        )
        if (fun == "sum") {
            assert(identical(sum(counts), sum(counts(object))))
        }
        rownames <- rownames(counts)

        # Return ---------------------------------------------------------------
        args <- list(
            assays = list(counts = counts),
            colData = colData(object)
        )
        if (is(object, "RangedSummarizedExperiment")) {
            args[["rowRanges"]] <- emptyRanges(names = rownames)
        } else {
            args[["rowData"]] <- DataFrame(row.names = rownames)
        }
        se <- do.call(what = SummarizedExperiment, args = args)
        validObject(se)
        se
    }

formals(aggregateRows.SummarizedExperiment)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("SummarizedExperiment"),
    definition = aggregateRows.SummarizedExperiment
)



# aggregateCols ================================================================
aggregateCols.matrix <-  # nolint
    function(
        object,
        groupings,
        fun
    ) {
        fun <- match.arg(fun)
        object %>%
            t() %>%
            aggregateRows(
                object = .,
                groupings = groupings,
                fun = fun
            ) %>%
            t()
    }

formals(aggregateCols.matrix)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("matrix"),
    definition = aggregateCols.matrix
)



aggregateCols.sparseMatrix <-  # nolint
    function(
        object,
        groupings,
        fun
    ) {
        fun <- match.arg(fun)
        object %>%
            Matrix::t(.) %>%
            aggregateRows(
                object = .,
                groupings = groupings,
                fun = fun
            ) %>%
            Matrix::t(.)
    }

formals(aggregateCols.sparseMatrix)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("sparseMatrix"),
    definition = aggregateCols.sparseMatrix
)



aggregateCols.SummarizedExperiment <-  # nolint
    function(object, col = "aggregate", fun) {
        validObject(object)
        assert(
            hasValidDimnames(object),
            isString(col)
        )
        fun <- match.arg(fun)

        # Groupings ------------------------------------------------------------
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
            validNames(levels(groupings))
        )
        names(groupings) <- colnames(object)

        # Assays ---------------------------------------------------------------
        counts <- aggregateCols(
            object = counts(object),
            groupings = groupings,
            fun = fun
        )
        if (fun == "sum") {
            assert(identical(sum(counts), sum(counts(object))))
        }

        # Column data ----------------------------------------------------------
        # Reslot with minimal sample-level data only.
        sampleNames <- sampleData(object)[["aggregate"]]
        assert(is.factor(sampleNames))
        sampleNames <- levels(sampleNames)
        sampleData <- DataFrame(
            sampleName = sampleNames,
            row.names = makeNames(sampleNames)
        )
        assert(identical(rownames(sampleData), colnames(counts)))

        # Collapse the sample data. This step will replace the `sampleName`
        # column with the `aggregate` column metadata.
        interestingGroups <- setdiff(interestingGroups(object), "sampleName")
        sampleData <- sampleData(object) %>%
            as_tibble() %>%
            select(!!!syms(unique(c("aggregate", interestingGroups)))) %>%
            unique() %>%
            mutate(rowname = makeNames(!!sym("aggregate"))) %>%
            rename(sampleName = !!sym("aggregate")) %>%
            arrange(!!!syms(c("rowname", "sampleName"))) %>%
            mutate_all(as.factor) %>%
            mutate_all(droplevels) %>%
            as("DataFrame")
        assert(hasRownames(sampleData))

        # Return ---------------------------------------------------------------
        args <- list(
            assays = list(counts = counts),
            colData = sampleData
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

formals(aggregateCols.SummarizedExperiment)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SummarizedExperiment"),
    definition = aggregateCols.SummarizedExperiment
)



aggregateCols.SingleCellExperiment <-  # nolint
    function(object, fun) {
        validObject(object)
        fun <- match.arg(fun)

        # Remap cellular barcode groupings -------------------------------------
        colData <- colData(object)
        assert(
            isSubset(c("sampleID", "aggregate"), colnames(colData)),
            is.factor(colData[["aggregate"]])
        )

        message(paste(
            "Remapping cells to aggregate samples:",
            toString(sort(levels(colData[["aggregate"]])))
        ))

        map <- colData(object) %>%
            as_tibble(rownames = "cellID") %>%
            select(!!!syms(c("cellID", "sampleID", "aggregate")))

        # Check to see if we can aggregate.
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

        # Reslot the `aggregate` column using these groupings.
        assert(identical(names(groupings), colnames(object)))
        colData(object)[["aggregate"]] <- groupings

        # Generate SingleCellExperiment ----------------------------------------
        # Using `SummarizedExperiment` method here.
        rse <- aggregateCols(
            object = as(object, "RangedSummarizedExperiment"),
            fun = fun
        )
        assert(is(rse, "RangedSummarizedExperiment"))

        # Update the sample data.
        colData <- colData(rse)
        colData[["sampleID"]] <- cell2sample
        colData[["sampleName"]] <- colData[["sampleID"]]

        # Now ready to generate aggregated SCE.
        sce <- makeSingleCellExperiment(
            assays = SummarizedExperiment::assays(rse),
            rowRanges = SummarizedExperiment::rowRanges(object),
            colData = SummarizedExperiment::colData(rse),
            metadata = list(
                aggregate = TRUE,
                aggregateCols = groupings,
                interestingGroups = interestingGroups(object)
            ),
            spikeNames = SingleCellExperiment::spikeNames(object)
        )
        validObject(sce)
        sce
    }

formals(aggregateCols.SingleCellExperiment)[["fun"]] <- .aggregateFuns

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SingleCellExperiment"),
    definition = aggregateCols.SingleCellExperiment
)
