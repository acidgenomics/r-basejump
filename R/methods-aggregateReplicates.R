#' Aggregate Replicates
#'
#' @rdname aggregateReplicates
#' @name aggregateReplicates
#' @family Data Management Utilities
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @inheritParams AllGenerics
#'
#' @param groupings Factor that defines the aggregation groupings. The new
#'   aggregation names are defined as the factor levels, and the original
#'   replicates are defined as the names of the factor.
#'
#' @return Object with aggregated counts per pooled sample (columns).
#'
#' @examples
#' counts <- data.frame(
#'     sample1_rep1 = c(0, 0, 0, 1, 2),
#'     sample1_rep2 = c(0, 0, 0, 3, 4),
#'     sample2_rep1 = c(1, 2, 0, 0, 0),
#'     sample2_rep2 = c(3, 4, 0, 0, 0)
#' )
#'
#' groupings <- factor(c("sample1", "sample1", "sample2", "sample2"))
#' names(groupings) <- colnames(counts)
#'
#' # matrix
#' mat <- as(counts, "matrix")
#' aggregateReplicates(mat, groupings = groupings)
#'
#' # dgCMatrix
#' dgc <- as(mat, "dgCMatrix")
#' aggregateReplicates(dgc, groupings = groupings)
NULL



# Constructors =================================================================
.aggregateReplicatesDenseMatrix <- function(object, groupings) {
    if (!is.factor(groupings)) {
        abort("`groupings` must be factor")
    }
    if (!identical(names(groupings), colnames(object))) {
        abort("`groupings` doesn't match colnames")
    }
    t <- t(object)
    rownames(t) <- groupings
    tagg <- rowsum(x = t, group = groupings, reorder = FALSE)
    agg <- t(tagg)
    agg
}



#' @importFrom Matrix.utils aggregate.Matrix
.aggregateReplicatesSparseMatrix <- function(object, groupings) {
    if (!is.factor(groupings)) {
        abort("`groupings` must be factor")
    }
    if (!identical(names(groupings), colnames(object))) {
        abort("`groupings` doesn't match colnames")
    }
    t <- Matrix::t(object)
    rownames(t) <- groupings
    tagg <- aggregate.Matrix(t, groupings = groupings, fun = "sum")
    agg <- Matrix::t(tagg)
    agg
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
    signature("matrix"),
    .aggregateReplicatesDenseMatrix)
