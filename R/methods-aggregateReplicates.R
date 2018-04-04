#' Aggregate Replicates
#'
#' @name aggregateReplicates
#' @family Math Functions
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @inheritParams general
#' @param groupings Factor that defines the aggregation groupings. The new
#'   aggregation names are defined as the factor levels, and the original
#'   replicates are defined as the names of the factor.
#'
#' @return Object with aggregated counts per pooled sample (columns).
#'
#' @examples
#' counts <- data.frame(
#'     "sample_1.1" = as.integer(c(0, 0, 0, 1, 2)),
#'     "sample_1.2" = as.integer(c(0, 0, 0, 3, 4)),
#'     "sample_2.1" = as.integer(c(1, 2, 0, 0, 0)),
#'     "sample_2.2" = as.integer(c(3, 4, 0, 0, 0))
#' )
#'
#' groupings <- factor(c("sample_1", "sample_1", "sample_2", "sample_2"))
#' names(groupings) <- colnames(counts)
#'
#' # matrix ====
#' mat <- as(counts, "matrix")
#' aggregateReplicates(mat, groupings = groupings)
#'
#' # dgCMatrix ====
#' dgc <- as(mat, "dgCMatrix")
#' aggregateReplicates(dgc, groupings = groupings)
NULL



# Methods ======================================================================
#' @rdname aggregateReplicates
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
