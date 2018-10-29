# nocov start
# nolint start



#' Deprecated Functions
#'
#' @name deprecated
#' @keywords internal
#'
#' @inheritParams basejump.globals::params
#'
#' @return [.Deprecated()].
NULL



#' Defunct Functions
#'
#' @name defunct
#' @keywords internal
#'
#' @inheritParams basejump.globals::params
#'
#' @return [.Defunct()].
NULL



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
flatFiles <- function(object) {
    .Deprecated("coerceS4ToList(from), as(object, \"list\"), or as.list(x)")
    coerceS4ToList(from = object)
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
