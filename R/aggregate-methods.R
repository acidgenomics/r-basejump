#' Aggregate Columns or Rows
#'
#' Aggregate sample replicates (columns) or gene/transcript features (rows).
#'
#' [aggregateReplicates()] works across the columns, and is designed to
#' aggregate sample replicates. [aggregateFeatures()] works down the rows, and
#' is designed to aggregate features (e.g. genes or transcripts). Most commonly,
#' the [aggregateFeatures()] function can be used to aggregate counts from
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
#' class(sparse)
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
#'     assay = sparse,
#'     colData = DataFrame(aggregate = samples),
#'     rowData = DataFrame(aggregate = genes)
#' )
#'
#' # aggregateReplicates ====
#' aggregateReplicates(matrix, groupings = samples)
#' aggregateReplicates(sparse, groupings = samples)
#' aggregateReplicates(se)
#'
#' # aggregateFeatures ====
#' aggregateFeatures(matrix, groupings = genes)
#' aggregateFeatures(sparse, groupings = genes)
#' aggregateReplicates(se)
NULL



.aggregateMessage <- function(groupings) {
    message(paste(
        "Groupings:",
        printString(groupings),
        sep = "\n"
    ))
}



# aggregateReplicates ==========================================================
#' @rdname aggregate
#' @export
setMethod(
    "aggregateReplicates",
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
    "aggregateReplicates",
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
    "aggregateReplicates",
    signature("SummarizedExperiment"),
    function(object) {
        validObject(object)
        assert_is_subset("aggregate", colnames(colData(object)))
        assert_is_subset("aggregate", colnames(sampleData(object)))
        groupings <- colData(object)[["aggregate"]]
        assert_is_factor(groupings)
        assertAllAreValidNames(
            x = as.character(groupings),
            severity = "warning"
        )
        groupings <- makeNames(groupings, unique = FALSE)
        groupings <- as.factor(groupings)
        names(groupings) <- colnames(object)

        # Assays ===============================================================
        message("Aggregating counts")
        counts <- aggregateReplicates(
            object = counts(object),
            groupings = groupings
        )
        assert_are_identical(sum(counts), sum(counts(object)))

        # Column data ==========================================================
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

        # Return ===============================================================
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



# aggregateFeatures ============================================================
#' @rdname aggregate
#' @export
setMethod(
    "aggregateFeatures",
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
    "aggregateFeatures",
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
    "aggregateFeatures",
    signature("SummarizedExperiment"),
    function(object) {
        stop("Not added yet")
    }
)
