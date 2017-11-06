#' Aggregate Replicates
#'
#' @rdname aggregateReplicates
#' @name aggregateReplicates
#' @family Data Management Utilities
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @inheritParams AllGenerics
#'
#' @param cells Cellular barcode identifiers. These are the colnames of the
#'   counts matrix.
#'
#' @return Object of same class with aggregated counts per pooled sample
#'   (columns).
NULL



# Constructors ====
#' @importFrom Matrix.utils aggregate.Matrix
.aggregateReplicatesSparse <- function(object, cells) {
    tsparse <- Matrix::t(object)
    rownames(tsparse) <- cells
    tsparse <- aggregate.Matrix(tsparse, groupings = cells, fun = "sum")
    sparse <- Matrix::t(tsparse)
    sparse
}



# Methods ====
#' @rdname aggregateReplicates
#' @export
setMethod(
    "aggregateReplicates",
    signature("dgCMatrix"),
    .aggregateReplicatesSparse)



#' @rdname aggregateReplicates
#' @export
setMethod(
    "aggregateReplicates",
    signature("dgTMatrix"),
    .aggregateReplicatesSparse)
