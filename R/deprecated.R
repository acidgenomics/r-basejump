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
metadataTable <- function() {
    .Deprecated("sampleMetadata")
}



#' @rdname deprecated
#' @export
.dataVersions <- function() {
    .Deprecated("readDataVersions")
}



#' @rdname deprecated
#' @export
.logFile <- function() {
    .Deprecated("readLogFile")
}



#' @rdname deprecated
#' @export
.programs <- function() {
    .Deprecated("readProgramVersions")
}



#' @rdname deprecated
#' @export
.sampleYAML <- function() {
    .Deprecated("sampleYAML")
}



#' @rdname deprecated
#' @export
.sampleYAMLMetadata <- function() {
    .Deprecated("sampleYAMLMetadata")
}



#' @rdname deprecated
#' @export
.sampleYAMLMetrics <- function(...) {
    .Deprecated("sampleYAMLMetrics")
    sampleYAMLMetrics(...)
}



# 0.0.25 =====
#' @rdname deprecated
#' @importFrom scales percent
#' @export
pct <- function(...) {
    scales::percent(...)
}



# 0.1.0 ====
#' @rdname deprecated
#' @export
sampleDirs <- function() {
    .Deprecated()
}
