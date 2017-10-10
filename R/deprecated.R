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
    .Deprecated("prepareSummarizedExperiment")
    prepareSummarizedExperiment(...)
}



#' @rdname deprecated
#' @export
prepareSE <- function(...) {
    .Deprecated("prepareSummarizedExperiment")
    prepareSummarizedExperiment(...)
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
.dataVersions <- function(...) {
    .Deprecated("readDataVersions")
    readDataVersions(...)
}



#' @rdname deprecated
#' @export
.logFile <- function(...) {
    .Deprecated("readLogFile")
    readLogFile(...)
}



#' @rdname deprecated
#' @export
.programs <- function(...) {
    .Deprecated("readProgramVersions")
    readProgramVersions(...)
}



#' @rdname deprecated
#' @export
.sampleYAML <- function(...) {
    .Deprecated("sampleYAML")
    sampleYAML(...)
}



#' @rdname deprecated
#' @export
.sampleYAMLMetadata <- function(...) {
    .Deprecated("sampleYAMLMetadata")
    sampleYAMLMetadata(...)
}



#' @rdname deprecated
#' @export
.sampleYAMLMetrics <- function(...) {
    .Deprecated("sampleYAMLMetrics")
    sampleYAMLMetrics(...)
}



# This is safe to remove once pushed to master
#' @rdname deprecated
#' @export
.SummarizedExperiment <- function(...) {  # nolint
    .Deprecated("prepareSummarizedExperiment")
    prepareSummarizedExperiment(...)
}
