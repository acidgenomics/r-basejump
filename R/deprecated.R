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
#' @importFrom scales percent
#' @export
pct <- function(...) {
    .Deprecated("scales::percent")
    scales::percent(...)
}



# v0.1.0 =======================================================================
#' #' @rdname deprecated
#' #' @export
#' sampleDirs <- function(...) {
#'     .Defunct()
#' }



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



 # v0.3.2 ======================================================================
 #' @rdname deprecated
 #' @export
 annotable <- function(...) {
     .Defunct("genes, transcripts, ensembl, rowRanges, or rowData")
 }

 assertIsAnnotable <- function(...) {
     .Deprecated("assertAreGeneAnnotations")
     assertAreGeneAnnotations(...)
 }

 sanitizeAnnotable <- function(...) {
     .Deprecated("sanitizeRowData")
     sanitizeRowData(...)
 }
