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
#' @note Updated 2020-01-30.
#'
#' @inheritParams acidroxygen::params
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
#' @param by `factor`.
#'   Defines the aggregation groupings. The new aggregate names are defined as
#'   the `factor` [levels][base::levels], and the original, unaggregated names
#'   are defined as the [names][base::names].
#' @param col `character(1)`.
#'   Name of column in either [`rowData()`][SummarizedExperiment::rowData] or
#'   [`colData()`][SummarizedExperiment::colData] that defines the desired
#'   aggregation groupings.
#' @param fun `character(1)`.
#'   Name of the aggregation method to apply.
#'   Note that "n" will return the number of aggregations (count).
#'   Uses [`match.arg()`][base::match.arg] internally.
#'
#' @seealso
#' - `stats::aggregate()`.
#' - `S4Vectors::aggregate()`.
#' - `Matrix.utils::aggregate.Matrix()`.
#' - `muscat::aggregateData()`.
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
#' class(counts)
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
#' ## sparseMatrix
#' sparse <- as(counts, "sparseMatrix")
#' class(sparse)
#' print(sparse)
#'
#' ## SummarizedExperiment
#' se <- SummarizedExperiment::SummarizedExperiment(
#'     assays = SimpleList(counts = counts),
#'     colData = DataFrame(
#'         sampleName = as.factor(names(samples)),
#'         aggregate = samples
#'     ),
#'     rowData = DataFrame(aggregate = genes)
#' )
#' print(se)
#'
#' ## aggregateRows ====
#' aggregateRows(counts, groupings = genes)
#' aggregateRows(sparse, groupings = genes)
#' aggregateRows(se)
#'
#' ## aggregateCols ====
#' aggregateCols(counts, groupings = samples)
#' aggregateCols(sparse, groupings = samples)
#' aggregateCols(se)
NULL



#' @rdname aggregate
#' @name aggregate
#' @importFrom S4Vectors aggregate
#' @usage aggregate(x, ...)
#' @export
NULL



## Matrix multiplication using sparse model (design matrix).
## Note that this works row-wise, like stats data.frame method.
## Updated 2020-01-30.
`aggregate,Matrix` <-  # nolint
    function(
        x,
        by,
        fun = c("sum", "mean", "n")
    ) {
        assert(
            hasRows(x), hasCols(x),
            is.factor(by),
            identical(names(by), rownames(x))
        )
        fun <- match.arg(fun)
        if (fun == "n") {
            x <- x != 0L
        }
        model <- fac2sparse(by)
        ## This step calculates the sum.
        result <- model %*% x
        if (fun == "mean") {
            n <- aggregate(x = x, by = by, fun = "n")
            result <- result / n
        }
        result
    }



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregate",
    signature = signature("Matrix"),
    definition = `aggregate,Matrix`
)
