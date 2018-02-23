#' Aggregate Replicates
#'
#' @rdname aggregateReplicates
#' @name aggregateReplicates
#' @family Aggregation Utilities
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @inheritParams general
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



# Methods ======================================================================
#' @rdname aggregateReplicates
#' @importFrom Matrix.utils aggregate.Matrix
#' @export
setMethod(
    "aggregateReplicates",
    signature("dgCMatrix"),
    function(object, groupings) {
        assert_is_factor(groupings)
        assert_are_identical(colnames(object), names(groupings))
        t <- Matrix::t(object)
        rownames(t) <- groupings
        tagg <- aggregate.Matrix(t, groupings = groupings, fun = "sum")
        agg <- Matrix::t(tagg)
        agg
    })



#' @rdname aggregateReplicates
#' @export
setMethod(
    "aggregateReplicates",
    signature("matrix"),
    function(object, groupings) {
        assert_is_factor(groupings)
        assert_are_identical(colnames(object), names(groupings))
        t <- t(object)
        rownames(t) <- groupings
        tagg <- rowsum(x = t, group = groupings, reorder = FALSE)
        agg <- t(tagg)
        agg
    })
