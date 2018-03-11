#' Aggregate Features
#'
#' @name aggregateFeatures
#' @family Math Functions
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @inheritParams general
#' @param groupings Feature groupings (e.g. gene or transcript IDs), defined as
#'   a named factor. The pooled features must be defined as the factor levels,
#'   and the original features as the names of the factor.
#'
#' @return Object of same class with aggregated features (rows).
#'
#' @examples
#' counts <- data.frame(
#'     "sample1" = c(0, 1, 2, 3),
#'     "sample2" = c(1, 2, 3, 4),
#'     row.names = c("gene1.1", "gene1.2", "gene2.1", "gene2.2")
#' )
#'
#' groupings <- factor(c("gene1", "gene1", "gene2", "gene2"))
#' names(groupings) <- rownames(counts)
#'
#' # matrix ====
#' mat <- as(counts, "matrix")
#' aggregateFeatures(mat, groupings = groupings)
#'
#' # dgCMatrix ====
#' dgc <- as(mat, "dgCMatrix")
#' aggregateFeatures(dgc, groupings = groupings)
NULL



# Methods ======================================================================
#' @rdname aggregateFeatures
#' @importFrom Matrix.utils aggregate.Matrix
#' @export
setMethod(
    "aggregateFeatures",
    signature("dgCMatrix"),
    function(object, groupings) {
        assert_is_factor(groupings)
        assert_are_identical(rownames(object), names(groupings))
        rownames(object) <- groupings
        aggregate.Matrix(object, groupings = groupings, fun = "sum")
    })



#' @rdname aggregateFeatures
#' @export
setMethod(
    "aggregateFeatures",
    signature("matrix"),
    function(object, groupings) {
        assert_is_factor(groupings)
        assert_are_identical(rownames(object), names(groupings))
        rowsum(object, group = groupings, reorder = FALSE)
    })
