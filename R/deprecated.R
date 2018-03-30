# nocov start

#' Defunct or Deprecated Functions
#'
#' @name deprecated
#' @keywords internal
#'
#' @inheritParams general
#'
#' @return [.Defunct()] or [.Deprecated()] calls.
NULL



# Legacy =======================================================================
#' @rdname deprecated
#' @export
summarizeRows <- function(...) {
    .Defunct("collapseToString")
}

#' @rdname deprecated
#' @export
wash <- function(...) {
    .Defunct()
}



# v0.0.23 ======================================================================
#' @rdname deprecated
#' @export
packageSE <- function(...) {
    .Defunct("bcbioBase::prepareSummarizedExperiment")
}



#' @rdname deprecated
#' @export
prepareSE <- function(...) {
    .Defunct("bcbioBase::prepareSummarizedExperiment")
}



# v0.0.24 ======================================================================
#' @rdname deprecated
#' @export
metadataTable <- function(...) {
    .Defunct("bcbioBase::sampleData")
}



# v0.0.25 ======================================================================
#' @rdname deprecated
#' @export
pct <- function(...) {
    .Defunct("scales::percent")
}



# v0.1.6 ======================================================================
#' @rdname deprecated
#' @export
fc2lr <- function(...) {
    .Defunct("foldChangeToLogRatio")
}

#' @rdname deprecated
#' @export
lr2fc <- function(...) {
    .Defunct("logRatioToFoldChange")
}



# v0.1.1 =======================================================================
#' @rdname deprecated
#' @export
comp <- function() {
    .Defunct("Biostrings::complement")
}

#' @rdname deprecated
#' @export
revcomp <- function() {
    .Defunct("Biostrings::reverseComplement")
}



# v0.2.2 =======================================================================
#' @rdname deprecated
symbol2gene <- function() {
    .Defunct()
}



# v0.3.0 =======================================================================
#' @rdname deprecated
#' @export
checkAnnotable <- function(...) {
    .Deprecated("assertIsAnnotable")
    assertIsAnnotable(...)
}

#' @rdname deprecated
#' @export
checkGene2symbol <- function(...) {
    .Deprecated("assertIsGene2symbol")
    assertIsGene2symbol(...)
}

#' @rdname deprecated
#' @export
checkTx2gene <- function(...) {
    .Deprecated("assertIsTx2gene")
    assertIsTx2gene(...)
}



# v0.3.1 =======================================================================
#' @rdname deprecated
#' @export
assertFormalHeaderLevel <- function(...) {
    .Deprecated("assertIsAHeaderLevel")
    assertIsAHeaderLevel(...)
}

#' @rdname deprecated
#' @export
 assertFormalColorFunction <- function(...) {
    .Deprecated("assertIsHexColorFunctionOrNULL")
    assertIsHexColorFunctionOrNULL(...)
}

#' @rdname deprecated
#' @export
initializeDir <- function(...) {
    .Deprecated("initializeDirectory")
    initializeDirectory(...)
}



 # v0.4.0 ======================================================================
#' @rdname deprecated
#' @export
annotable <- function(...) {
    .Defunct("makeGRangesFromEnsembl")
}

#' @rdname deprecated
#' @export
assertIsAnnotable <- function(...) {
     .Deprecated("assertAreGeneAnnotations")
     assertAreGeneAnnotations(...)
}

#' @rdname deprecated
#' @export
ensembl <- function(...) {
    .Deprecated("makeGRangesFromEnsembl")
    makeGRangesFromEnsembl(...)
}

#' @rdname deprecated
#' @export
gene2symbolFromGFF <- function(...) {
    .Deprecated("makeGene2symbolFromGFF")
    makeGene2symbolFromGFF(...)
}

#' @rdname deprecated
#' @export
gene2symbolFromGFF -> gene2symbolFromGTF

#' @rdname deprecated
#' @export
sanitizeAnnotable <- function(...) {
    .Deprecated("sanitizeRowData")
    sanitizeRowData(...)
}

#' @rdname deprecated
#' @export
tx2geneFromGFF <- function(...) {
    .Deprecated("makeTx2geneFromGFF")
    makeTx2geneFromGFF(...)
}

#' @rdname deprecated
#' @export
tx2geneFromGFF -> tx2geneFromGTF

# nocov end
