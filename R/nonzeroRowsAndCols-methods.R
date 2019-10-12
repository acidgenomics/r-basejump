#' @name nonzeroRowsAndCols
#' @inherit bioverbs::nonzeroRowsAndCols
#' @note Updated 2019-10-09.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' dim(object)
#' x <- nonzeroRowsAndCols(object)
#' dim(x)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' dim(object)
#' x <- nonzeroRowsAndCols(object)
#' dim(x)
NULL



#' @rdname nonzeroRowsAndCols
#' @name nonzeroRowsAndCols
#' @importFrom bioverbs nonzeroRowsAndCols
#' @usage nonzeroRowsAndCols(object, ...)
#' @export
NULL



## Updated 2019-08-23.
`nonzeroRowsAndCols,matrix` <-  # nolint
    function(object) {
        originalDim <- dim(object)
        nzrows <- rowSums(object) > 0L
        nzcols <- colSums(object) > 0L
        object <- object[nzrows, nzcols, drop = FALSE]
        dim <- dim(object)
        if (!identical(dim, originalDim)) {
            message(sprintf(
                fmt = paste(
                    "Filtered zero count rows and columns:",
                    "  - %d / %d %s (%s)",
                    "  - %d / %d %s (%s)",
                    sep = "\n"
                ),
                ## Rows.
                dim[[1L]],
                originalDim[[1L]],
                ngettext(
                    n = dim[[1L]],
                    msg1 = "row",
                    msg2 = "rows"
                ),
                percent(dim[[1L]] / originalDim[[1L]]),
                ## Columns.
                dim[[2L]],
                originalDim[[2L]],
                ngettext(
                    n = dim[[2L]],
                    msg1 = "column",
                    msg2 = "columns"
                ),
                percent(dim[[2L]] / originalDim[[2L]])
            ))
        }
        object
    }



#' @rdname nonzeroRowsAndCols
#' @export
setMethod(
    f = "nonzeroRowsAndCols",
    signature = signature("matrix"),
    definition = `nonzeroRowsAndCols,matrix`
)



## Updated 2019-08-23.
`nonzeroRowsAndCols,Matrix` <-  # nolint
    appendToBody(
        fun = `nonzeroRowsAndCols,matrix`,
        values = list(
            quote(rowSums <- Matrix::rowSums),
            quote(colSums <- Matrix::colSums)
        )
    )



#' @rdname nonzeroRowsAndCols
#' @export
setMethod(
    f = "nonzeroRowsAndCols",
    signature = signature("Matrix"),
    definition = `nonzeroRowsAndCols,Matrix`
)



## nolint start

## DelayedArray methods disabled until bug fix:
## https://github.com/Bioconductor/DelayedArray/issues/55

## Updated 2019-10-09.
## > `nonzeroRowsAndCols,DelayedArray` <-  # nolint
## >     appendToBody(
## >         fun = `nonzeroRowsAndCols,matrix`,
## >         values = list(
## >             quote(rowSums <- DelayedMatrixStats::rowSums2),
## >             quote(colSums <- DelayedMatrixStats::colSums2)
## >         )
## >     )

## > #' @rdname nonzeroRowsAndCols
## > #' @export
## > setMethod(
## >     f = "nonzeroRowsAndCols",
## >     signature = signature("DelayedArray"),
## >     definition = `nonzeroRowsAndCols,DelayedArray`
## > )

## nolint end



## Updated 2019-09-16.
`nonzeroRowsAndCols,SummarizedExperiment` <-  # nolint
    function(object, assay = 1L) {
        assay <- assay(object, i = assay)
        assay <- nonzeroRowsAndCols(assay)
        object <- object[rownames(assay), colnames(assay)]
        object
    }



#' @rdname nonzeroRowsAndCols
#' @export
setMethod(
    f = "nonzeroRowsAndCols",
    signature = signature("SummarizedExperiment"),
    definition = `nonzeroRowsAndCols,SummarizedExperiment`
)
