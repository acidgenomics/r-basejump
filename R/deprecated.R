## nocov start
## nolint start



#' @name defunct
#' @inherit acidroxygen::defunct description examples return seealso title
#' @inheritParams acidroxygen::params
#' @keywords internal
NULL



#' @name deprecated
#' @inherit acidroxygen::deprecated description examples return seealso title
#' @inheritParams acidroxygen::params
#' @keywords internal
NULL



## v0.8.0 =======================================================================
#' @rdname defunct
#' @export
aggregateFeatures <- function(...) {
    .Defunct("aggregateRows")
}

#' @rdname defunct
#' @export
aggregateReplicates <- function(...) {
    .Defunct("aggregateCols")
}

#' @rdname defunct
#' @export
aggregateSamples <- function(...) {
    .Defunct("aggregateCols")
}

#' @rdname defunct
#' @export
broadClass <- function(...) {
    .Defunct("makeGRanges")
}

#' @rdname defunct
#' @export
checkClasses <- function(...) {
    .Defunct("validateClasses")
}

#' @rdname defunct
#' @export
eggnog <- function(...) {
    .Defunct("EggNOG")
}

#' @rdname defunct
#' @export
ensembl2entrez <- function(...) {
    .Defunct("Ensembl2Entrez")
}

#' @rdname deprecated
#' @export
fixNA <- function(...) {
    .Deprecated("sanitizeNA")
    sanitizeNA(...)
}

#' @rdname defunct
#' @export
gene2symbol <- function(...) {
    .Defunct("Gene2Symbol")
}

#' @rdname defunct
#' @export
grepString <- function(...) {
    .Defunct()
}

#' @rdname defunct
#' @export
hgnc2gene <- function(...) {
    .Defunct("HGNC2Ensembl")
}

#' @rdname defunct
#' @export
hgnc2ensembl <- function(...) {
    .Defunct("HGNC2Ensembl")
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

#' @rdname defunct
#' @export
makeGene2symbolFromEnsembl <- function(...) {
    .Defunct("makeGene2SymbolFromEnsembl")
}

#' @rdname defunct
#' @export
makeGene2symbolFromGFF <- function(...) {
    .Defunct("makeGene2SymbolFromGFF")
}

#' @rdname defunct
#' @export
makeGene2symbolFromGTF <- function(...) {
    .Defunct("makeGene2SymbolFromGTF")
}

#' @rdname defunct
#' @export
makeTx2geneFromEnsembl <- function(...) {
    .Defunct("makeTx2GeneFromEnsembl")
}

#' @rdname defunct
#' @export
makeTx2geneFromGFF <- function(...) {
    .Defunct("makeTx2GeneFromGFF")
}

#' @rdname defunct
#' @export
makeTx2geneFromGTF <- function(...) {
    .Defunct("makeTx2GeneFromGTF")
}

#' @rdname defunct
#' @export
mgi2gene <- function(...) {
    .Defunct("MGI2Ensembl")
}

#' @rdname defunct
#' @export
mgi2ensembl <- function(...) {
    .Defunct("MGI2Ensembl")
}

#' @rdname defunct
#' @export
panther <- function(...) {
    .Defunct("PANTHER")
}

#' @rdname defunct
#' @export
setArgsToDoCall <- function(...) {
    .Defunct("matchArgsToDoCall")
}

#' @rdname defunct
#' @export
tx2gene <- function(...) {
    .Defunct("Tx2Gene")
}



## v0.8.5 =======================================================================
#' @rdname deprecated
#' @param plotlist `list`.
#'   List containing `ggplot` objects.
#' @export
markdownPlotlist <- function(plotlist, ...) {
    .Deprecated("markdownPlots")
    markdownPlots(list = plotlist, ...)
}

#' @rdname deprecated
#' @export
mdPlotlist <- markdownPlotlist



## v0.10.4 ======================================================================
## Now recommending `plotCounts()` instead of `plotGene()`.
## Still in use by some revdeps, so keep re-exported.
#' @importFrom bioverbs plotGene
#' @export
bioverbs::plotGene



## v0.10.9 ======================================================================
#' @rdname defunct
#' @export
theme_midnight <- function(...) {
    .Defunct("acidplots::acid_theme_dark")
}

#' @rdname defunct
#' @export
theme_paperwhite <- function(...) {
    .Defunct("acidplots::acid_theme_light")
}

#' @rdname defunct
#' @export
tx2geneFromGFF <- function(...) {
    .Defunct("makeTx2GeneFromGFF")
}



# v0.11.6 ======================================================================
#' @rdname defunct
#' @export
separatorBar <- function(...) {
    .Defunct("separator")
}



# v0.11.8 ======================================================================
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

#' @importFrom transformer relevel
#' @export
transformer::relevel

#' @rdname defunct
#' @export
relevelColData <- function(...) {
    .Defunct("droplevels")
}

#' @rdname defunct
#' @export
relevelRowData <- function(...) {
    .Defunct("droplevels")
}

#' @rdname defunct
#' @export
relevelRowRanges <- function(...) {
    .Defunct("droplevels")
}



# v0.11.11 =====================================================================
#' @rdname deprecated
#' @export
readSampleData <- function(...) {
    .Deprecated("importSampleData")
    importSampleData(...)
}

#' @rdname deprecated
#' @export
readTx2Gene <- function(...) {
    .Deprecated("importTx2Gene")
    importTx2Gene(...)
}



## nolint end
## nocov end
