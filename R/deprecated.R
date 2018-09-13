# nocov start



#' Deprecated Functions
#'
#' @name deprecated
#' @author Michael Steinbaugh
#' @keywords internal
#'
#' @inheritParams general
#'
#' @return [.Deprecated()].
NULL



#' Defunct Functions
#'
#' @name defunct
#' @author Michael Steinbaugh
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

#' @rdname deprecated
#' @export
initializeDir <- function(...) {
    .Deprecated("initializeDirectory")
    initializeDirectory(...)
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

# #' @rdname defunct
# #' @export
# tx2gene <- function(...) {
#     .Defunct("makeTx2geneFromGFF or bcbioBase::readTx2gene")
# }

#' @rdname deprecated
#' @export
tx2geneFromGFF <- function(...) {
    .Deprecated("makeTx2geneFromGFF")
    makeTx2geneFromGFF(...)
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



# v0.6.2 =======================================================================
#' @rdname defunct
#' @export
setMethod(
    f = "broadClass",
    signature = signature("data.frame"),
    definition = function(object) {
        .Defunct(
            msg = "`broadClass()` now requires a `GRanges` class object"
        )
    }
)

#' @rdname defunct
#' @export
setMethod(
    f = "broadClass",
    signature = signature("DataFrame"),
    definition = getMethod("broadClass", "data.frame")
)



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
assertIsCharacterOrNULL <- function(object, ...) {
    .Deprecated("assert_is_any_of")
    assert_is_any_of(
        x = object,
        classes = c("character", "NULL"),
        ...
    )
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

#' @rdname deprecated
#' @export
fixNA <- function(...) {
    .Deprecated("sanitizeNA")
    sanitizeNA(...)
}

#' @rdname deprecated
#' @export
readFileByExtension <- function(...) {
    .Deprecated("import")
    import(...)
}

#' @rdname defunct
#' @export
setArgsToDoCall <- function(...) {
    .Defunct("matchArgsToDoCall")
}



# nocov end
