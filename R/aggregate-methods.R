#' Aggregate Rows or Columns
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
#' @family Count Matrix Functions
#' @name aggregate
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @inheritParams general
#' @param groupings `factor`. Defines the aggregation groupings.
#'   The new aggregate names are defined as the `factor` `levels`, and the
#'   original, unaggregated names are defined as the `names`.
#' @param col `string`. Name of column in either [rowData()] or [colData()] that
#'   defines the desired aggregation groupings.
#' @param fun `string`. Name of the aggregation function. Uses [match.arg()].
#'
#' @return Modified object, with aggregated rows (features) or columns
#'   (samples).
#'
#' @seealso
#' - [stats::aggregate()].
#' - [S4Vectors::aggregate()].
#' - [Matrix.utils::aggregate.Matrix()].
#'
#' @examples
#' # Example data ====
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
#' # matrix
#' matrix <- as(counts, "matrix")
#' class(matrix)
#' print(matrix)
#'
#' # sparseMatrix
#' sparse <- as(matrix, "sparseMatrix")
#' class(sparse)
#' print(sparse)
#'
#' # SummarizedExperiment
#' se <- SummarizedExperiment::SummarizedExperiment(
#'     assay = list(counts = sparse),
#'     colData = S4Vectors::DataFrame(
#'         sampleName = names(samples),
#'         aggregate = samples
#'     ),
#'     rowData = S4Vectors::DataFrame(aggregate = genes)
#' )
#' print(se)
#'
#' # aggregateRows ====
#' aggregateRows(matrix, groupings = genes)
#' aggregateRows(sparse, groupings = genes)
#' aggregateRows(se)
#'
#' # aggregateCols ====
#' aggregateCols(matrix, groupings = samples)
#' aggregateCols(sparse, groupings = samples)
#' aggregateCols(se)
NULL



.aggregateFuns <- c("sum", "mean")



# Don't message when aggregating a large factor.
.aggregateMessage <- function(groupings, fun) {
    assert_is_factor(groupings)
    assert_is_a_string(fun)
    msg <- paste0(
        "Aggregating counts using `", fun, "()`..."
    )
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



# matrix =======================================================================
.aggregateRows.matrix <-  # nolint
    function(
        object,
        groupings,
        fun
    ) {
        assertHasValidDimnames(object)
        assert_is_factor(groupings)
        assert_are_identical(rownames(object), names(groupings))
        assertAllAreValidNames(levels(groupings))
        fun <- match.arg(fun)
        .aggregateMessage(groupings, fun = fun)
        # stats::aggregate.data.frame S3 method.
        aggregate(
            x = object,
            by = list(rowname = groupings),
            FUN = getFromNamespace(x = fun, ns = "base")
        ) %>%
            column_to_rownames() %>%
            as.matrix()
    }
formals(.aggregateRows.matrix)[["fun"]] <- .aggregateFuns



.aggregateCols.matrix <-  # nolint
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
formals(.aggregateCols.matrix)[["fun"]] <- .aggregateFuns



# sparseMatrix =================================================================
.aggregateRows.sparseMatrix <-  # nolint
    function(
        object,
        groupings,
        fun
    ) {
        validObject(object)
        assertHasValidDimnames(object)
        assert_is_factor(groupings)
        assert_are_identical(rownames(object), names(groupings))
        assertAllAreValidNames(levels(groupings))
        fun <- match.arg(fun)
        .aggregateMessage(groupings, fun = fun)
        # Matrix.utils::aggregate.Matrix S3 method.
        aggregate(
            x = object,
            groupings = groupings,
            fun = fun
        )
    }
formals(.aggregateRows.sparseMatrix)[["fun"]] <- .aggregateFuns



.aggregateCols.sparseMatrix <-  # nolint
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
formals(.aggregateCols.sparseMatrix)[["fun"]] <- .aggregateFuns



# SummarizedExperiment =========================================================
.aggregateRows.SE <-  # nolint
    function(object, col = "aggregate", fun) {
        validObject(object)
        assertHasValidDimnames(object)
        assert_is_a_string(col)
        fun <- match.arg(fun)

        # Groupings ------------------------------------------------------------
        assert_is_subset(col, colnames(rowData(object)))
        groupings <- rowData(object)[[col]]
        assert_is_factor(groupings)
        assertAllAreValidNames(levels(groupings))
        names(groupings) <- rownames(object)

        # Assays ---------------------------------------------------------------
        counts <- aggregateRows(
            object = counts(object),
            groupings = groupings,
            fun = fun
        )
        if (fun == "sum") {
            assert_are_identical(
                x = sum(counts),
                y = sum(counts(object))
            )
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
formals(.aggregateRows.SE)[["fun"]] <- .aggregateFuns



.aggregateCols.SE <-  # nolint
    function(object, col = "aggregate", fun) {
        validObject(object)
        assertHasValidDimnames(object)
        assert_is_a_string(col)
        fun <- match.arg(fun)

        # Groupings ------------------------------------------------------------
        assert_is_subset(col, colnames(colData(object)))
        assert_is_subset(col, colnames(sampleData(object)))
        groupings <- colData(object)[[col]]
        assert_is_factor(groupings)
        assertAllAreValidNames(levels(groupings))
        names(groupings) <- colnames(object)

        # Assays ---------------------------------------------------------------
        counts <- aggregateCols(
            object = counts(object),
            groupings = groupings,
            fun = fun
        )
        if (fun == "sum") {
            assert_are_identical(
                x = sum(counts),
                y = sum(counts(object))
            )
        }

        # Column data ----------------------------------------------------------
        # Reslot with minimal sample-level data only.
        sampleNames <- sampleData(object)[["aggregate"]]
        assert_is_factor(sampleNames)
        sampleNames <- levels(sampleNames)
        sampleData <- DataFrame(
            sampleName = sampleNames,
            row.names = makeNames(sampleNames)
        )
        assert_are_identical(
            x = rownames(sampleData),
            y = colnames(counts)
        )

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
        assertHasRownames(sampleData)

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
formals(.aggregateCols.SE)[["fun"]] <- .aggregateFuns



# SingleCellExperiment =========================================================
.aggregateCols.SCE <-  # nolint
    function(object, fun) {
        validObject(object)
        fun <- match.arg(fun)

        # Remap cellular barcode groupings -------------------------------------
        colData <- colData(object)
        assert_is_subset(c("sampleID", "aggregate"), colnames(colData))
        assert_is_factor(colData[["aggregate"]])

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
        assert_are_identical(names(groupings), colnames(object))
        colData(object)[["aggregate"]] <- groupings

        # Generate SingleCellExperiment ----------------------------------------
        # Using `SummarizedExperiment` method here.
        rse <- aggregateCols(
            object = as(object, "RangedSummarizedExperiment"),
            fun = fun
        )
        assert_is_all_of(rse, "RangedSummarizedExperiment")

        # Update the sample data.
        colData <- colData(rse)
        colData[["sampleID"]] <- cell2sample
        colData[["sampleName"]] <- colData[["sampleID"]]

        # Now ready to generate aggregated SCE.
        sce <- makeSingleCellExperiment(
            assays = assays(rse),
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
formals(.aggregateCols.SCE)[["fun"]] <- .aggregateFuns



# Methods ======================================================================
#' @describeIn aggregate Aggregate rows or columns using a grouping `factor`.
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("matrix"),
    definition = .aggregateRows.matrix
)



#' @describeIn aggregate Same conventions as `matrix`.
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("sparseMatrix"),
    definition = .aggregateRows.sparseMatrix
)



#' @describeIn aggregate Aggregate rows or columns in [assays()] using an
#'   automatically generated grouping `factor`, which is obtained from a
#'   user-defined column (`col` argument) in either [rowData()] or [colData()].
#'   Slot an `aggregate` column into [rowData()] for `aggregateRows()`, or into
#'   [colData()] for [aggregateCols()]. This method will define the `groupings`
#'   automatically and perform the aggregation.
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("SummarizedExperiment"),
    definition = .aggregateRows.SE
)



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("matrix"),
    definition = .aggregateCols.matrix
)



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("sparseMatrix"),
    definition = .aggregateCols.sparseMatrix
)



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SummarizedExperiment"),
    definition = .aggregateCols.SE
)



#' @describeIn aggregate Aggregate [assays()] across cell-level groupings,
#'   defined by a column in [colData()]. Inherits from `SummarizedExperiment`,
#'   and still relies upon slotting an `aggregate` column into [colData()]. Note
#'   that these groupings will map to cells, so care must be taken to properly
#'   aggregate samples.
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SingleCellExperiment"),
    definition = .aggregateCols.SCE
)
