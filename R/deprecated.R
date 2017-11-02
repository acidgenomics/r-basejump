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



# v0.0.23 ====
#' @rdname deprecated
#' @export
packageSE <- function(...) {
    .Deprecated("prepareSummarizedExperiment")
    prepareSummarizedExperiment(...)
}



#' @rdname deprecated
#' @export
prepareSE <- function(...) {
    .Deprecated("prepareSummarizedExperiment")
    prepareSummarizedExperiment(...)
}



# v0.0.24 ====
#' @rdname deprecated
#' @export
metadataTable <- function() {
    .Deprecated("sampleMetadata")
}



# v0.0.25 =====
#' @rdname deprecated
#' @importFrom scales percent
#' @export
pct <- function(...) {
    .Deprecated("scales::percent")
    scales::percent(...)
}



# v0.1.0 ====
#' @rdname deprecated
#' @export
sampleDirs <- function() {
    .Deprecated()
}
