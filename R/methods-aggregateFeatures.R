#' Aggregate Features
#'
#' @rdname aggregateFeatures
#' @name aggregateFeatures
#' @family Data Management Utilities
#' @author Rory Kirchner, Michael Steinbaugh
#'
#' @param features Feature identifiers (e.g. gene or transcript IDs). These are
#'   the rownames of the counts matrix.
#'
#' @return Object of same class with aggregated features (rows).
NULL



# Constructors ====
#' @importFrom Matrix.utils aggregate.Matrix
.aggregateFeaturesSparse <- function(object, features) {
    rownames(object) <- features
    object <- object[!is.na(rownames(object)), , drop = FALSE]
    aggregate.Matrix(object, groupings = rownames(object), fun = "sum")
}



# Methods ====
#' @rdname aggregateFeatures
#' @export
setMethod(
    "aggregateFeatures",
    signature("dgCMatrix"),
    .aggregateFeaturesSparse)



#' @rdname aggregateFeatures
#' @export
setMethod(
    "aggregateFeatures",
    signature("dgTMatrix"),
    .aggregateFeaturesSparse)
