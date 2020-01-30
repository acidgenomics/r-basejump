#' Aggregate
#'
#' @name aggregate
#' @note Updated 2020-01-30.
#'
#' @inheritParams acidroxygen::params
#' @param by `factor`.
#'   Aggregation groupings. The new aggregate names are defined as the `factor`
#'   [levels][base::levels], and the original, unaggregated names are defined as
#'   the [names][base::names].
#' @param fun `character(1)`.
#'   Name of the aggregation function to apply.
#'   Uses [`match.arg()`][base::match.arg] internally.
#' @param ... Additional arguments.
#'
#' @seealso
#' - `stats::aggregate()`.
#' - `S4Vectors::aggregate()`.
#' - `Matrix.utils::aggregate.Matrix()` (defunct).
#' - `muscat::aggregateData()`.
#'
#' @return Modified object.
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
#' ## Matrix
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
