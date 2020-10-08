#' Correlation
#'
#' @name correlation
#' @inherit AcidGenerics::correlation
#' @note Updated 2020-01-20.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams stats::cor
#' @param i `integer(1)` or `character(1)`.
#'   For `SummarizedExperiment`, primary assay.
#' @param j `integer(1)`, `character(1)`, or `NULL`.
#'   For `SummarizedExperiment`, optional secondary assay.
#'   If `NULL`, calculates correlation matrix only on the primary assay.
#' @param ... Additional arguments.
#'
#' @examples
#' data(correlation, package = "AcidTest")
#' list <- correlation
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
#' correlation(x = x, i = 1L)
#' correlation(x = x, i = 1L, j = 2L)
#' correlation(x = x, y = y)
NULL



method <- formals(stats::cor)[["method"]]



## Updated 2020-01-20.
`correlation,numeric,numeric` <-  # nolint
    function(x, y, method) {
        assert(
            hasLength(x),
            identical(length(x), length(y)),
            !anyNA(x), !anyNA(y)
        )
        method <- match.arg(method)
        n <- length(x)
        cli_alert(sprintf(
            fmt = "Calculating %s correlation on %d %s.",
            method,
            n,
            ngettext(
                n = n,
                msg1 = "value",
                msg2 = "values"
            )
        ))
        cor(x = x, y = y, method = method)
    }

formals(`correlation,numeric,numeric`)[["method"]] <- method



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



## Updated 2020-01-20.
`correlation,matrix,missing` <-  # nolint
    function(x, y = NULL, method) {
        assert(!anyNA(x))
        method <- match.arg(method)
        cli_alert(sprintf(
            "Calculating {.val %s} correlation matrix.", method
        ))
        cor(x = x, y = NULL, method = method)
    }

formals(`correlation,matrix,missing`)[["method"]] <- method



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



## Updated 2019-11-08.
`correlation,matrix,matrix` <-  # nolint
    function(x, y, method) {
        method <- match.arg(method)
        correlation(x = c(x), y = c(y), method = method)
    }

formals(`correlation,matrix,matrix`)[["method"]] <- method



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



## Updated 2019-11-08.
`correlation,SE,missing` <-  # nolint
    function(
        x, y = NULL,
        i = 1L, j = NULL,
        method
    ) {
        assert(!identical(i, j))
        method <- match.arg(method)
        if (is.null(j)) {
            correlation(
                x = assay(x, i = i),
                method = method
            )
        } else {
            correlation(
                x = assay(x, i = i),
                y = assay(x, i = j),
                method = method
            )
        }
    }

formals(`correlation,SE,missing`)[["method"]] <- method



#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "SummarizedExperiment",
        y = "missingOrNULL"
    ),
    definition = `correlation,SE,missing`
)



## Updated 2019-11-08.
`correlation,SE,SE` <-  # nolint
    function(x, y, i = 1L, method) {
        method <- match.arg(method)
        correlation(
            x = assay(x, i = i),
            y = assay(y, i = i),
            method = method
        )
    }

formals(`correlation,SE,SE`)[["method"]] <- method



#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "SummarizedExperiment",
        y = "SummarizedExperiment"
    ),
    definition = `correlation,SE,SE`
)



rm(method)
