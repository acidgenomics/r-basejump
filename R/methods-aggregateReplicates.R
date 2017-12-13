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



# Constructors =================================================================
#' @importFrom stats setNames
.aggregateReplicatesDenseMatrix <- function(
    object,
    pattern = "_L\\d+") {
    # Obtain the unique pooled sample names
    if (!all(grepl(pattern, colnames(object)))) {
        stop("Lane pattern didn't match all samples")
    }
    stem <- gsub(x = colnames(object),
                 pattern = pattern,
                 replacement = "") %>%
        unique() %>%
        sort()
    # Perform [rowSums()] on the matching columns per sample
    lapply(seq_along(stem), function(a) {
        object %>%
            .[, grepl(paste0("^", stem[a], pattern), colnames(.))] %>%
            rowSums()
    }) %>%
        setNames(stem) %>%
        do.call(cbind, .) %>%
        # Need to round here, otherwise DESeq2 will fail
        round()
}



#' @importFrom Matrix.utils aggregate.Matrix
.aggregateReplicatesSparseMatrix <- function(object, cells) {
    if (!identical(length(cells), ncol(object))) {
        stop("'cells' length must match the number of columns",
             call. = FALSE)
    }
    tsparse <- Matrix::t(object)
    rownames(tsparse) <- cells
    tsparse <- aggregate.Matrix(tsparse, groupings = cells, fun = "sum")
    sparse <- Matrix::t(tsparse)
    sparse
}



# Methods ======================================================================
#' @rdname aggregateReplicates
#' @export
setMethod(
    "aggregateReplicates",
    signature("dgCMatrix"),
    .aggregateReplicatesSparseMatrix)



#' @rdname aggregateReplicates
#' @export
setMethod(
    "aggregateReplicates",
    signature("dgTMatrix"),
    .aggregateReplicatesSparseMatrix)



#' @rdname aggregateReplicates
#' @export
setMethod(
    "aggregateReplicates",
    signature("matrix"),
    .aggregateReplicatesDenseMatrix)
