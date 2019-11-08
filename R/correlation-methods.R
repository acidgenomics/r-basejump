#' Correlation
#'
#' @name correlation
#' @inherit bioverbs::correlation
#' @note Updated 2019-11-08.
#'
#' @inheritParams acidroxygen::params
#' @param i `integer(1)` or `character(1)`.
#'   For `SummarizedExperiment`, primary assay.
#' @param j `integer(1)`, `character(1)`, or `NULL`.
#'   For `SummarizedExperiment`, optional secondary assay.
#'   If `NULL`, calculates correlation matrix only on the primary assay.
#'
#' @examples
#' data(correlation, package = "acidtest")
#' list <- correlation
#' rm(correlation)
#'
#' ## vector ====
#' x <- list[["vector_x"]]
#' y <- list[["vector_y"]]
#'
#' head(x)
#' head(y)
#'
#' correlation(x = x, y = y)
#'
#' ## matrix ====
#' x <- list[["matrix_x"]]
#' y <- list[["matrix_y"]]
#'
#' head(x)
#' head(y)
#'
#' cor(x)
#' correlation(x)
#'
#' cor(x = c(x), y = c(y))
#' correlation(x, y)
#'
#' ## SummarizedExperiment ====
#' x <- list[["SummarizedExperiment_x"]]
#' y <- list[["SummarizedExperiment_y"]]
#'
#' correlation(x = x, i = 1L, j = 2L)
NULL



#' @rdname correlation
#' @name correlation
#' @importFrom bioverbs correlation
#' @usage correlation(x, y, ...)
#' @export
NULL



`correlation,numeric,numeric` <-  # nolint
    function(x, y, method) {
        assert(
            hasLength(x),
            identical(length(x), length(y)),
            !anyNA(x),
            !anyNA(y)
        )
        cor(x = x, y = y, method = match.arg(method))
    }

formals(`correlation,numeric,numeric`)[["method"]] <-
    formals(stats::cor)[["method"]]



#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "numeric",
        y = "numeric"
    ),
    definition = `correlation,numeric,numeric`
)



## Updated 2019-11-07.
`correlation,matrix,NULL` <-  # nolint
    function(x, y, method) {
        cor(x = x, y = y, match.arg(method))
    }

formals(`correlation,matrix,NULL`)[["method"]] <-
    formals(`correlation,numeric,numeric`)[["method"]]



#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "matrix",
        y = "NULL"
    ),
    definition = `correlation,matrix,NULL`
)



## Updated 2019-11-07.
`correlation,matrix,matrix` <-  # nolint
    function(x, y, method) {
        correlation(x = c(x), y = c(y), method = match.arg(method))
    }

formals(`correlation,matrix,matrix`)[["method"]] <-
    formals(`correlation,matrix,NULL`)[["method"]]



#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "matrix",
        y = "matrix"
    ),
    definition = `correlation,matrix,matrix`
)
