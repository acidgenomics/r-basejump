#' Deprecated Functions
#'
#' @rdname deprecated
#' @name deprecated
#' @keywords internal
#'
#' @inheritParams AllGenerics
#'
#' @return Soft deprecation to new functions.
NULL



# 0.0.23 ====
#' @rdname deprecated
#' @export
packageSE <- function(...) {
    .Deprecated(".SummarizedExperiment")
    .SummarizedExperiment(...)
}



#' @rdname deprecated
#' @export
prepareSE <- function(...) {
    .Deprecated(".SummarizedExperiment")
    .SummarizedExperiment(...)
}



# 0.0.24 ====
#' @rdname deprecated
#' @export
metadataTable <- function(...) {
    .Deprecated("sampleMetadata")
    sampleMetadata(...)
}



#' @rdname deprecated
#' @export
prepareSummarizedExperiment <- function(...) {
    .Deprecated(".SummarizedExperiment")
    .SummarizedExperiment(...)
}
