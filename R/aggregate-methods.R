# FIXME Add `SummarizedExperiment` explanation in documentation.



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
#' counts <- DataFrame(
#'     sample1_replicate1 = as.integer(c(0, 1, 1, 1)),
#'     sample1_replicate2 = as.integer(c(1, 0, 1, 1)),
#'     sample2_replicate1 = as.integer(c(1, 1, 0, 1)),
#'     sample2_replicate2 = as.integer(c(1, 1, 1, 0)),
#'     row.names = paste0("transcript", seq_len(4))
#' )
#' print(counts)
#'
#' # Dense matrix
#' matrix <- as(counts, "matrix")
#' class(matrix)
#' print(matrix)
#'
#' # Sparse matrix
#' sparse <- as(matrix, "sparseMatrix")
#' class(sparse)
#' print(sparse)
#'
#' samples <- factor(c("sample1", "sample1", "sample2", "sample2"))
#' names(samples) <- colnames(counts)
#' print(samples)
#'
#' genes <- factor(c("gene1", "gene1", "gene2", "gene2"))
#' names(genes) <- rownames(counts)
#' print(genes)
#'
#' # SummarizedExperiment
#' se <- SummarizedExperiment(
#'     assay = list(counts = sparse),
#'     colData = DataFrame(
#'         sampleName = names(samples),
#'         aggregate = samples
#'     ),
#'     rowData = DataFrame(aggregate = genes)
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



.aggregateMessage <- function(groupings) {
    message(paste(
        "Groupings:",
        printString(groupings),
        sep = "\n"
    ))
}



# aggregateCols ================================================================
#' @rdname aggregate
#' @export
setMethod(
    "aggregateCols",
    signature("matrix"),
    function(object, groupings) {
        assert_is_factor(groupings)
        assertAllAreValidNames(as.character(groupings))
        assert_are_identical(colnames(object), names(groupings))
        .aggregateMessage(groupings)
        t <- t(object)
        rownames(t) <- groupings
        tagg <- rowsum(x = t, group = groupings, reorder = FALSE)
        agg <- t(tagg)
        agg
    })



#' @rdname aggregate
#' @export
setMethod(
    "aggregateCols",
    signature("sparseMatrix"),
    function(object, groupings) {
        assert_is_factor(groupings)
        assertAllAreValidNames(as.character(groupings))
        assert_are_identical(colnames(object), names(groupings))
        .aggregateMessage(groupings)
        t <- t(object)
        rownames(t) <- groupings
        tagg <- aggregate.Matrix(t, groupings = groupings, fun = "sum")
        agg <- t(tagg)
        agg
    })



#' @rdname aggregate
#' @export
setMethod(
    "aggregateCols",
    signature("SummarizedExperiment"),
    function(object) {
        validObject(object)
        assert_is_subset("aggregate", colnames(colData(object)))
        assert_is_subset("aggregate", colnames(sampleData(object)))
        groupings <- colData(object)[["aggregate"]]
        assert_is_factor(groupings)
        assertAllAreValidNames(as.character(groupings))
        names(groupings) <- colnames(object)

        # Assays ---------------------------------------------------------------
        message("Aggregating counts")
        counts <- aggregateCols(
            object = counts(object),
            groupings = groupings
        )
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
        do.call(
            what = SummarizedExperiment,
            args = args
        )
    }
)



# aggregateRows ================================================================
#' @rdname aggregate
#' @export
setMethod(
    "aggregateRows",
    signature("matrix"),
    function(object, groupings) {
        assert_is_factor(groupings)
        assert_are_identical(rownames(object), names(groupings))
        .aggregateMessage(groupings)
        rowsum(object, group = groupings, reorder = FALSE)
    })



#' @rdname aggregate
#' @export
setMethod(
    "aggregateRows",
    signature("sparseMatrix"),
    function(object, groupings) {
        assert_is_factor(groupings)
        assert_are_identical(rownames(object), names(groupings))
        .aggregateMessage(groupings)
        rownames(object) <- groupings
        aggregate.Matrix(object, groupings = groupings, fun = "sum")
    })



#' @rdname aggregate
#' @export
setMethod(
    "aggregateRows",
    signature("SummarizedExperiment"),
    function(object) {
        validObject(object)
        assert_is_subset("aggregate", colnames(rowData(object)))
        groupings <- rowData(object)[["aggregate"]]
        assert_is_factor(groupings)
        assertAllAreValidNames(as.character(groupings))
        names(groupings) <- rownames(object)

        # Assays ---------------------------------------------------------------
        message("Aggregating counts")
        counts <- aggregateRows(
            object = counts(object),
            groupings = groupings
        )
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
        do.call(
            what = SummarizedExperiment,
            args = args
        )
    }
)
