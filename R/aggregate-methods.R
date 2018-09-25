# TODO Improve SCE method documentation.



#' Aggregate Columns or Rows
#'
#' Aggregate sample replicates (columns) or gene/transcript features (rows).
#'
#' [aggregateCols()] works across the columns, and is designed to aggregate
#' sample replicates. [aggregateRows()] works down the rows, and is designed to
#' aggregate features (e.g. genes or transcripts). Most commonly, the
#' [aggregateRows()] function can be used to aggregate counts from
#' transcript-level to gene-level.
#'
#' @section SummarizedExperiment:
#'
#' Slot an `aggregate` column into [SummarizedExperiment::colData()] for
#' [aggregateCols()], or into [SummarizedExperiment::rowData()] for
#' `aggregateRows()`. The S4 method will define the `groupings` automatically
#' and perform the aggregation.
#'
#' @name aggregate
#' @family Data Functions
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @inheritParams general
#' @param groupings `factor`. Defines the aggregation groupings.
#'   The new aggregate names are defined as the `factor` `levels`, and the
#'   original, unaggregated names are defined as the `names`.
#'
#' @return Modified object, with aggregated columns (samples) or rows
#'   (features).
#'
#' @examples
#' # Example data ====
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
#' counts <- data.frame(
#'     sample1_replicate1 = as.integer(c(0, 1, 1, 1)),
#'     sample1_replicate2 = as.integer(c(1, 0, 1, 1)),
#'     sample2_replicate1 = as.integer(c(1, 1, 0, 1)),
#'     sample2_replicate2 = as.integer(c(1, 1, 1, 0)),
#'     row.names = paste0("transcript", seq_len(4))
#' )
#' print(counts)
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
#' # aggregateCols ====
#' aggregateCols(matrix, groupings = samples)
#' aggregateCols(sparse, groupings = samples)
#' aggregateCols(se)
#'
#' # aggregateRows ====
#' aggregateRows(matrix, groupings = genes)
#' aggregateRows(sparse, groupings = genes)
#' aggregateRows(se)
NULL



# Don't message when aggregating single cell data.
.aggregateMessage <- function(groupings) {
    assert_is_factor(groupings)
    if (length(groupings) <= 20L) {
        message(paste("Groupings:", printString(groupings), sep = "\n"))
    }
}



# aggregateCols ================================================================
.aggregateCols.matrix <-  # nolint
    function(object, groupings) {
        assertHasValidDimnames(object)
        assert_is_factor(groupings)
        assert_are_identical(colnames(object), names(groupings))
        assertAllAreValidNames(levels(groupings))
        .aggregateMessage(groupings)
        t <- t(object)
        rownames(t) <- groupings
        tagg <- rowsum(x = t, group = groupings, reorder = FALSE)
        agg <- t(tagg)
        agg
    }



.aggregateCols.sparseMatrix <-  # nolint
    function(object, groupings) {
        validObject(object)
        assertHasValidDimnames(object)
        assert_is_factor(groupings)
        assert_are_identical(colnames(object), names(groupings))
        assertAllAreValidNames(levels(groupings))
        .aggregateMessage(groupings)
        t <- t(object)
        rownames(t) <- groupings
        tagg <- aggregate.Matrix(t, groupings = groupings, fun = "sum")
        agg <- t(tagg)
        agg
    }



.aggregateCols.SE <-  # nolint
    function(object) {
        validObject(object)
        assertHasValidDimnames(object)
        assert_is_subset("aggregate", colnames(colData(object)))
        assert_is_subset("aggregate", colnames(sampleData(object)))
        if ("sampleNameAggregate" %in% colnames(colData)) {
            stop("Use `aggregate` instead of `sampleNameAggregate`")
        }
        groupings <- colData(object)[["aggregate"]]
        assert_is_factor(groupings)
        assertAllAreValidNames(levels(groupings))
        names(groupings) <- colnames(object)

        # Assays ---------------------------------------------------------------
        message("Aggregating counts")
        counts <- aggregateCols(counts(object), groupings = groupings)
        assert_are_identical(sum(counts), sum(counts(object)))

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
        do.call(what = SummarizedExperiment, args = args)
    }



.aggregateCols.SCE <-  # nolint
    function(object) {
        validObject(object)

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
            stop("Cell IDs are not prefixed with sample IDs")
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
        rse <- aggregateCols(as(object, "RangedSummarizedExperiment"))
        assert_is_all_of(rse, "RangedSummarizedExperiment")

        # Update the sample data.
        colData <- colData(rse)
        colData[["sampleID"]] <- cell2sample
        colData[["sampleName"]] <- colData[["sampleID"]]

        # Now ready to generate aggregated SCE.
        makeSingleCellExperiment(
            assays = assays(rse),
            rowRanges = rowRanges(object),
            colData = colData(rse),
            metadata = list(
                aggregateCols = groupings,
                cell2sample = cell2sample,
                interestingGroups = interestingGroups(object)
            ),
            spikeNames = spikeNames(object)
        )
    }



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



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SingleCellExperiment"),
    definition = .aggregateCols.SCE
)



# aggregateRows ================================================================
.aggregateRows.matrix <-  # nolint
    function(object, groupings) {
        assertHasValidDimnames(object)
        assert_is_factor(groupings)
        assert_are_identical(rownames(object), names(groupings))
        assertAllAreValidNames(levels(groupings))
        .aggregateMessage(groupings)
        rowsum(object, group = groupings, reorder = FALSE)
    }



.aggregateRows.sparseMatrix <-  # nolint
    function(object, groupings) {
        validObject(object)
        assertHasValidDimnames(object)
        assert_is_factor(groupings)
        assert_are_identical(rownames(object), names(groupings))
        assertAllAreValidNames(levels(groupings))
        .aggregateMessage(groupings)
        rownames(object) <- groupings
        aggregate.Matrix(object, groupings = groupings, fun = "sum")
    }



.aggregateRows.SE <-  # nolint
    function(object) {
        validObject(object)
        assertHasValidDimnames(object)
        assert_is_subset("aggregate", colnames(rowData(object)))
        groupings <- rowData(object)[["aggregate"]]
        assert_is_factor(groupings)
        assertAllAreValidNames(levels(groupings))
        names(groupings) <- rownames(object)

        # Assays ---------------------------------------------------------------
        message("Aggregating counts")
        counts <- aggregateRows(counts(object), groupings = groupings)
        assert_are_identical(sum(counts), sum(counts(object)))
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
        do.call(what = SummarizedExperiment, args = args)
    }



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("matrix"),
    definition = .aggregateRows.matrix
)



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("sparseMatrix"),
    definition = .aggregateRows.sparseMatrix
)



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("SummarizedExperiment"),
    definition = .aggregateRows.SE
)



# Aliases ======================================================================
# Consider deprecating these in a future release.
#' @rdname aggregate
#' @usage NULL
#' @export
aggregateReplicates <- function(...) {
    aggregateCols(...)
}

#' @rdname aggregate
#' @usage NULL
#' @export
aggregateFeatures <- function(...) {
    aggregateRows(...)
}

#' @rdname aggregate
#' @usage NULL
#' @export
aggregateSamples <- function(...) {
    aggregateCols(...)
}
