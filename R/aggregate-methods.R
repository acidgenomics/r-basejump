#' Aggregate
#'
#' @name aggregate
#' @note Updated 2020-05-22.
#'
#' @inheritParams AcidRoxygen::params
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
#' counts <- matrix(
#'     data = c(
#'         0L, 2L, 2L, 2L,
#'         2L, 0L, 2L, 2L,
#'         2L, 2L, 0L, 2L,
#'         2L, 2L, 2L, 0L
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
#'
#' genes <- factor(paste0("gene", rep(seq_len(2L), each = 2L)))
#' names(genes) <- rownames(counts)
#' print(genes)
#'
#' ## matrix ====
#' print(counts)
#' aggregate(counts, by = genes)
#'
#' ## Matrix ====
#' sparse <- as(counts, "sparseMatrix")
#' print(sparse)
#' aggregate(sparse, by = genes)
NULL



## Using the `stats::aggregate.data.frame()` S3 method internally here.
## Updated 2020-05-22.
`aggregate,matrix` <-  # nolint
    function(
        x,
        by,
        fun = c("sum", "mean", "median", "geometricMean", "n")
    ) {
        assert(
            hasDimnames(x),
            is.factor(by),
            identical(rownames(x), names(by))
        )
        fun <- match.arg(fun)
        if (fun == "n") {
            x <- x != 0L
            mode(x) <- "integer"
            fun <- "sum"
        }
        x <- aggregate(
            x = as.data.frame(x),
            by = list(rowname = by),
            FUN = get(x = fun, inherits = TRUE)
        )
        rownames(x) <- x[["rowname"]]
        x[["rowname"]] <- NULL
        x <- as.matrix(x)
        x
    }



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregate",
    signature = signature("matrix"),
    definition = `aggregate,matrix`
)



## Matrix multiplication using sparse model (design matrix).
## Note that this works row-wise, like stats data.frame method.
## Updated 2020-05-22.
`aggregate,Matrix` <-  # nolint
    function(
        x,
        by,
        fun = c("sum", "mean", "n")
    ) {
        assert(
            hasDimnames(x),
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
            ## Avoid NaN from diving by zero.
            n[n == 0L] <- 1L
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
