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



# Legacy =======================================================================
#' @rdname deprecated
#' @export
summarizeRows <- function() {
    .Deprecated("collapseToString")
}

#' @rdname deprecated
#' @export
wash <- function() {
    .Deprecated()
}



# v0.0.23 ======================================================================
#' @rdname deprecated
#' @export
packageSE <- function() {
    .Deprecated("bcbioBase::prepareSummarizedExperiment")
}



#' @rdname deprecated
#' @export
prepareSE <- function() {
    .Deprecated("bcbioBase::prepareSummarizedExperiment")
}



# v0.0.24 ======================================================================
#' @rdname deprecated
#' @export
metadataTable <- function() {
    .Deprecated("bcbioBase::sampleMetadata")
}



# v0.0.25 ======================================================================
#' @rdname deprecated
#' @importFrom scales percent
#' @export
pct <- function(...) {
    .Deprecated("scales::percent")
    scales::percent(...)
}



# v0.1.0 =======================================================================
#' @rdname deprecated
#' @export
sampleDirs <- function() {
    .Deprecated()
}



# v0.1.6 ======================================================================
#' @rdname deprecated
#' @export
fc2lr <- function(...) {
    .Deprecated("foldChangeToLogRatio")
    foldChangeToLogRatio(...)
}

#' @rdname deprecated
#' @export
lr2fc <- function(...) {
    .Deprecated("logRatioToFoldChange")
    logRatioToFoldChange(...)
}



# v0.1.1 =======================================================================
#' @rdname deprecated
#' @export
comp <- function() {
    .Deprecated("Biostrings::complement")
}

#' @rdname deprecated
#' @export
revcomp <- function() {
    .Deprecated("Biostrings::reverseComplement")
}



# v0.2.2 =======================================================================
#' @rdname deprecated
symbol2gene <- function() {
    .Deprecated()
}
