# nocov start
# nolint start



#' Deprecated Functions
#'
#' @name deprecated
#' @keywords internal
#'
#' @inheritParams general
#'
#' @return [.Deprecated()].
NULL



#' Defunct Functions
#'
#' @name defunct
#' @keywords internal
#'
#' @inheritParams general
#'
#' @return [.Defunct()].
NULL



# v0.1.6 ======================================================================
#' @rdname defunct
#' @export
fc2lr <- function(...) {
    .Defunct("foldChangeToLogRatio")
}

#' @rdname defunct
#' @export
lr2fc <- function(...) {
    .Defunct("logRatioToFoldChange")
}



# v0.1.1 =======================================================================
#' @rdname defunct
#' @export
comp <- function(...) {
    .Defunct("Biostrings::complement")
}

#' @rdname defunct
#' @export
revcomp <- function(...) {
    .Defunct("Biostrings::reverseComplement")
}



# v0.2.2 =======================================================================
#' @rdname defunct
symbol2gene <- function(...) {
    .Defunct("convertSymbolsToGenes")
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



 # v0.4.0 ======================================================================
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
    .Deprecated("makeTx2GeneFromGFF")
    makeTx2GeneFromGFF(...)
}

#' @rdname deprecated
#' @export
tx2geneFromGFF -> tx2geneFromGTF



# v0.5.0 =======================================================================
#' @rdname deprecated
#' @export
multiassignAsNewEnvir <- function(...) {
    .Deprecated("multiassignAsEnvir")
    multiassignAsEnvir(...)
}



# v0.5.2 =======================================================================
#' @rdname deprecated
#' @export
geomean <- function(...) {
    .Deprecated("geometricMean")
    geometricMean(...)
}



# v0.5.4 =======================================================================
#' @rdname defunct
#' @export
assertIsGFF <- function(...) {
    .Defunct()
}

#' @rdname defunct
#' @export
parseGFFAttributes <- function(...) {
    .Defunct("readGFF")
}



# v0.5.8 =======================================================================
#' @rdname defunct
#' @export
dynamicPlotlist <- function(...) {
    .Defunct()
}



# v0.5.11 ======================================================================
#' @rdname deprecated
#' @export
midnightTheme <- function(...) {
    .Deprecated("theme_midnight")
    theme_midnight(...)
}

#' @rdname deprecated
#' @export
paperwhiteTheme <- function(...) {
    .Deprecated("theme_paperwhite")
    theme_paperwhite(...)
}



# v0.6.3 =======================================================================
#' @rdname defunct
#' @export
synonyms <- function(...) {
    .Defunct("geneSynonyms")
}



# v0.7.3 =======================================================================
#' @rdname deprecated
#' @export
assertIsURL <- function(...) {
    .Deprecated("assertAllAreURL")
    assertAllAreURL(...)
}



# v0.99.0 ======================================================================
#' @rdname deprecated
#' @export
aggregateFeatures <- function(...) {
    .Deprecated("aggregateRows")
    aggregateRows(...)
}

#' @rdname deprecated
#' @export
aggregateReplicates <- function(...) {
    .Deprecated("aggregateCols")
    aggregateCols(...)
}

#' @rdname deprecated
#' @export
aggregateSamples <- function(...) {
    .Deprecated("aggregateCols")
    aggregateCols(...)
}

#' @rdname deprecated
#' @export
assertFormalGene2symbol <- function(...) {
    .Deprecated("assertFormalGene2Symbol")
    assertFormalGene2Symbol(...)
}

#' @rdname deprecated
#' @export
assertIsAHeaderLevel <- function(...) {
    .Deprecated("assertIsHeaderLevel")
    assertIsHeaderLevel(...)
}

#' @rdname deprecated
#' @export
assertIsCharacterOrNULL <- function(object, ...) {
    .Deprecated("assert_is_any_of")
    assert_is_any_of(
        x = object,
        classes = c("character", "NULL"),
        ...
    )
}

#' @rdname defunct
#' @export
assertIsGene2symbol <- function(...) {
    .Defunct(msg = "Create `Gene2Symbol` class object.")
}

#' @rdname defunct
#' @export
assertIsTx2gene <- function(...) {
    .Defunct(msg = "Create `Tx2Gene` class object.")
}

#' @rdname deprecated
#' @export
assertIsDataFrameOrNULL <- function(object, ...) {
    .Deprecated("assert_is_any_of")
    assert_is_any_of(
        x = object,
        classes = c("data.frame", "NULL"),
        ...
    )
}

#' @rdname defunct
#' @export
broadClass <- function(object, ...) {
    .Defunct("makeGRanges")
}

#' @rdname deprecated
#' @export
fixNA <- function(...) {
    .Deprecated("sanitizeNA")
    sanitizeNA(...)
}

#' @rdname deprecated
#' @export
flatFiles <- function(object) {
    .Deprecated("coerceS4ToList(from), as(object, \"list\"), or as.list(x)")
    coerceS4ToList(from = object)
}

#' @rdname deprecated
#' @export
grepString <- function(...) {
    .Defunct("")
}

#' @rdname deprecated
#' @export
hgnc2gene <- function(...) {
    .Deprecated("hgnc2ensembl")
    hgnc2ensembl(...)
}

#' @rdname deprecated
#' @export
kables <- function(...) {
    .Deprecated("markdownTables")
    import(...)
}

#' @rdname deprecated
#' @export
makeGene2symbolFromEnsembl <- function(...) {
    .Deprecated("makeGene2SymbolFromEnsembl")
    makeGene2SymbolFromEnsembl(...)
}

#' @rdname deprecated
#' @export
makeGene2symbolFromGFF <- function(...) {
    .Deprecated("makeGene2SymbolFromGFF")
    makeGene2SymbolFromGFF(...)
}

#' @rdname deprecated
#' @export
makeGene2symbolFromGTF <- function(...) {
    .Deprecated("makeGene2SymbolFromGTF")
    makeGene2SymbolFromGTF(...)
}

#' @rdname deprecated
#' @export
makeTx2geneFromEnsembl <- function(...) {
    .Deprecated("makeTx2GeneFromEnsembl")
    makeTx2GeneFromEnsembl(...)
}

#' @rdname deprecated
#' @export
makeTx2geneFromGFF <- function(...) {
    .Deprecated("makeTx2GeneFromGFF")
    makeTx2GeneFromGFF(...)
}

#' @rdname deprecated
#' @export
makeTx2geneFromGTF <- function(...) {
    .Deprecated("makeTx2GeneFromGTF")
    makeTx2GeneFromGTF(...)
}

#' @rdname deprecated
#' @export
mgi2gene <- function(...) {
    .Deprecated("mgi2ensembl")
    mgi2ensembl(...)
}

#' @rdname deprecated
#' @export
readFileByExtension <- function(...) {
    .Deprecated("import")
    import(...)
}

#' @rdname deprecated
#' @export
readGFF <- function(...) {
    .Deprecated("import")
    import(...)
}

#' @rdname deprecated
#' @export
readGTF <- function(...) {
    .Deprecated("import")
    import(...)
}

#' @rdname deprecated
#' @export
readJSON <- function(...) {
    .Deprecated("import")
    import(...)
}

#' @rdname deprecated
#' @export
readYAML <- function(...) {
    .Deprecated("import")
    import(...)
}

#' @rdname defunct
#' @export
separatorBar <- function(...) {
    .Defunct("separator")
}

#' @rdname defunct
#' @export
setArgsToDoCall <- function(...) {
    .Defunct("matchArgsToDoCall")
}



# nolint end
# nocov end
