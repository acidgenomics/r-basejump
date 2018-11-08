# nocov start
# nolint start



#' Deprecated Functions
#'
#' @name deprecated
#' @keywords internal
#'
#' @inheritParams params
#'
#' @return [.Deprecated()].
NULL



#' Defunct Functions
#'
#' @name defunct
#' @keywords internal
#'
#' @inheritParams params
#'
#' @return [.Defunct()].
NULL



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



# v0.8.0 =======================================================================
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
assertAllAreValidNames <- function(...) {
    .Deprecated("assertAreValidNames")
    assertAreValidNames(...)
}

#' @rdname defunct
#' @export
assertAreGeneAnnotations <- function(...) {
    .Defunct(msg = "Create `Gene2Symbol` class object.")
}

#' @rdname defunct
#' @export
assertAreTranscriptAnnotations <- function(...) {
    .Defunct(msg = "Create `Tx2Gene` class object.")
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

#' @rdname defunct
#' @export
assertIsCharacterOrNULL <- function(object, ...) {
    .Defunct("assert_is_any_of")
}

#' @rdname defunct
#' @export
assertIsDataFrameOrNULL <- function(object, ...) {
    .Defunct("assert_is_any_of")
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
assertIsURL <- function(...) {
    .Deprecated("assertAllAreURL")
    assertAllAreURL(...)
}

#' @rdname defunct
#' @export
broadClass <- function(object, ...) {
    .Defunct("makeGRanges")
}

#' @rdname deprecated
#' @export
eggnog <- function(...) {
    .Deprecated("EggNOG")
    EggNOG(...)
}

#' @rdname deprecated
#' @export
ensembl2entrez <- function(...) {
    .Deprecated("Ensembl2Entrez")
    Ensembl2Entrez(...)
}

#' @rdname deprecated
#' @export
fixNA <- function(...) {
    .Deprecated("sanitizeNA")
    sanitizeNA(...)
}

#' @rdname deprecated
#' @export
gene2symbol <- function(...) {
    .Deprecated("Gene2Symbol")
    Gene2Symbol(...)
}

#' @rdname deprecated
#' @export
grepString <- function(...) {
    .Defunct()
}

#' @rdname deprecated
#' @export
hgnc2gene <- function(...) {
    .Deprecated("HGNC2Ensembl")
    HGNC2Ensembl(...)
}

#' @rdname deprecated
#' @export
hgnc2ensembl <- function(...) {
    .Deprecated("HGNC2Ensembl")
    HGNC2Ensembl(...)
}

#' @rdname deprecated
#' @export
initializeDirectory <- function(...) {
    .Deprecated("initDir")
    initDir(...)
}

#' @rdname deprecated
#' @export
kables <- function(...) {
    .Deprecated("markdownTables")
    markdownTables(...)
}

#' @rdname deprecated
#' @export
makeGene2symbolFromEnsembl <- function(...) {
    .Deprecated("makeGene2SymbolFromEnsembl")
    do.call(
        what = makeGene2SymbolFromEnsembl,
        args = matchArgsToDoCall()
    )
}

#' @rdname deprecated
#' @export
makeGene2symbolFromGFF <- function(...) {
    .Deprecated("makeGene2SymbolFromGFF")
    do.call(
        what = makeGene2SymbolFromGFF,
        args = matchArgsToDoCall()
    )
}

#' @rdname deprecated
#' @export
makeGene2symbolFromGTF <- function(...) {
    .Deprecated("makeGene2SymbolFromGTF")
    do.call(
        what = makeGene2SymbolFromGTF,
        args = matchArgsToDoCall()
    )
}

#' @rdname deprecated
#' @export
makeTx2geneFromEnsembl <- function(...) {
    .Deprecated("makeTx2GeneFromEnsembl")
    do.call(
        what = makeTx2GeneFromEnsembl,
        args = matchArgsToDoCall()
    )
}

#' @rdname deprecated
#' @export
makeTx2geneFromGFF <- function(...) {
    .Deprecated("makeTx2GeneFromGFF")
    do.call(
        what = makeTx2GeneFromGFF,
        args = matchArgsToDoCall()
    )
}

#' @rdname deprecated
#' @export
makeTx2geneFromGTF <- function(...) {
    .Deprecated("makeTx2GeneFromGTF")
    do.call(
        what = makeTx2GeneFromGTF,
        args = matchArgsToDoCall()
    )
}

#' @rdname deprecated
#' @export
mgi2gene <- function(...) {
    .Deprecated("mgi2ensembl")
    mgi2ensembl(...)
}

#' @rdname deprecated
#' @export
mgi2ensembl <- function(...) {
    .Deprecated("MGI2Ensembl")
    MGI2Ensembl(...)
}

#' @rdname deprecated
#' @export
panther <- function(...) {
    .Deprecated("PANTHER")
    PANTHER(...)
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

#' @rdname deprecated
#' @export
tx2gene <- function(...) {
    .Deprecated("Tx2Gene")
    Tx2Gene(...)
}



# nolint end
# nocov end
