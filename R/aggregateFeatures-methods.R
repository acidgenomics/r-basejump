#' Aggregate Features
#'
#' @name aggregateFeatures
#' @family Data Functions
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @inheritParams general
#' @param groupings `character`. Feature groupings (e.g. gene or transcript
#'   IDs), defined as a named factor. The pooled features must be defined as the
#'   factor levels, and the original features as the names of the factor.
#'
#' @return Object of same class with aggregated features (rows).
#'
#' @examples
#' counts <- DataFrame(
#'     "sample1" = as.integer(c(0, 1, 0, 2)),
#'     "sample2" = as.integer(c(1, 0, 2, 0)),
#'     row.names = paste0("transcript", seq_len(4))
#' )
#'
#' groupings <- factor(c("gene1", "gene1", "gene2", "gene2"))
#' names(groupings) <- rownames(counts)
#' print(groupings)
#'
#' # matrix ====
#' mat <- as(counts, "matrix")
#' print(mat)
#' aggregateFeatures(mat, groupings = groupings)
#'
#' # dgCMatrix ====
#' dgc <- as(mat, "dgCMatrix")
#' print(dgc)
#' aggregateFeatures(dgc, groupings = groupings)
NULL



#' @rdname aggregateFeatures
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
