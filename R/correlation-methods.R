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
#' stats::cor(x)
#' correlation(x)
#'
#' stats::cor(x = c(x), y = c(y))
#' correlation(x = x, y = y)
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
            !anyNA(x), !anyNA(y)
        )
        method <- match.arg(method)
        message("Calculating ", method, " correlation value.")
        cor(x = x, y = y, method = method)
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



`correlation,matrix,missing` <-  # nolint
    function(x, y, method) {
        method <- match.arg(method)
        message("Calculating ", method, " correlation matrix.")
        cor(x = x, y = NULL, method = method)
    }

formals(`correlation,matrix,missing`)[["method"]] <-
    formals(`correlation,numeric,numeric`)[["method"]]



#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "matrix",
        y = "missingOrNULL"
    ),
    definition = `correlation,matrix,missing`
)



## Updated 2019-11-07.
`correlation,matrix,matrix` <-  # nolint
    function(x, y, method) {
        correlation(x = c(x), y = c(y), method = match.arg(method))
    }

formals(`correlation,matrix,matrix`)[["method"]] <-
    formals(`correlation,matrix,missing`)[["method"]]



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



`correlation,Matrix,missing` <-  # nolint
    `correlation,matrix,missing`



#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "Matrix",
        y = "missingOrNULL"
    ),
    definition = `correlation,Matrix,missing`
)



`correlation,Matrix,Matrix` <-  # nolint
    `correlation,matrix,matrix`



#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "Matrix",
        y = "Matrix"
    ),
    definition = `correlation,Matrix,Matrix`
)
