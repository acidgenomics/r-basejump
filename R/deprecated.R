# nocov start
# nolint start



#' Deprecated functions
#'
#' @name deprecated
#' @keywords internal
#'
#' @inheritParams params
#'
#' @return `.Deprecated`.
NULL



#' Defunct functions
#'
#' @name defunct
#' @keywords internal
#'
#' @inheritParams params
#'
#' @return `.Defunct`.
NULL



# v0.4.0 ======================================================================
#' @rdname defunct
#' @export
ensembl <- function(...) {
    .Defunct("makeGRangesFromEnsembl")
}

#' @rdname defunct
#' @export
gene2symbolFromGFF <- function(...) {
    .Defunct("makeGene2symbolFromGFF")
}

#' @rdname defunct
#' @export
gene2symbolFromGFF -> gene2symbolFromGTF

#' @rdname defunct
#' @export
sanitizeAnnotable <- function(...) {
    .Defunct("atomize")
}

#' @rdname defunct
#' @export
tx2geneFromGFF <- function(...) {
    .Defunct("makeTx2GeneFromGFF")
}

#' @rdname defunct
#' @export
tx2geneFromGFF -> tx2geneFromGTF



# v0.5.0 =======================================================================
#' @rdname defunct
#' @export
multiassignAsNewEnvir <- function(...) {
    .Defunct("multiassignAsEnvir")
}



# v0.5.2 =======================================================================
#' @rdname defunct
#' @export
geomean <- function(...) {
    .Defunct("geometricMean")
}



# v0.5.4 =======================================================================
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
#' @rdname defunct
#' @export
midnightTheme <- function(...) {
    .Defunct(msg = paste0(
        "'midnightTheme' is defunct.\n",
        "Requiring snake case format for ggplot2 functions.\n",
        "Use 'theme_midnight' instead.\n",
        "See help(\"Defunct\")"
    ))
}

#' @rdname defunct
#' @export
paperwhiteTheme <- function(...) {
    .Defunct(msg = paste0(
        "'paperwhiteTheme' is defunct.\n",
        "Requiring snake case format for ggplot2 functions.\n",
        "Use 'theme_paperwhite' instead.\n",
        "See help(\"Defunct\")"
    ))
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
setGeneric(
    name = "aggregateFeatures",
    def = function(object, ...) {
        standardGeneric("aggregateFeatures")
    }
)

#' @rdname deprecated
#' @export
setMethod(
    f = "aggregateFeatures",
    signature = signature("ANY"),
    definition = function(object, ...) {
        # .Deprecated("aggregateRows")
        aggregateRows(object, ...)
    }
)

#' @rdname deprecated
#' @export
setGeneric(
    name = "aggregateReplicates",
    def = function(object, ...) {
        standardGeneric("aggregateReplicates")
    }
)

#' @rdname deprecated
#' @export
setMethod(
    f = "aggregateReplicates",
    signature = signature("ANY"),
    definition = function(object, ...) {
        # .Deprecated("aggregateCols")
        aggregateCols(object, ...)
    }
)

#' @rdname deprecated
#' @export
setGeneric(
    name = "aggregateSamples",
    def = function(object, ...) {
        standardGeneric("aggregateSamples")
    }
)

#' @rdname deprecated
#' @export
setMethod(
    f = "aggregateSamples",
    signature = signature("ANY"),
    definition = function(object, ...) {
        # .Deprecated("aggregateCols")
        aggregateCols(object, ...)
    }
)

#' @rdname defunct
#' @export
broadClass <- function(object, ...) {
    .Defunct("makeGRanges")
}

#' @rdname deprecated
#' @export
checkClasses <- function(...) {
    # .Deprecated("validateClasses")
    validateClasses(...)
}

#' @rdname deprecated
#' @export
eggnog <- function(...) {
    # .Deprecated("EggNOG")
    EggNOG(...)
}

#' @rdname deprecated
#' @export
ensembl2entrez <- function(...) {
    # .Deprecated("Ensembl2Entrez")
    Ensembl2Entrez(...)
}

#' @rdname deprecated
#' @export
fixNA <- function(...) {
    # .Deprecated("sanitizeNA")
    sanitizeNA(...)
}

#' @rdname deprecated
#' @export
gene2symbol <- function(...) {
    # .Deprecated("Gene2Symbol")
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
    # .Deprecated("HGNC2Ensembl")
    HGNC2Ensembl(...)
}

#' @rdname deprecated
#' @export
hgnc2ensembl <- function(...) {
    # .Deprecated("HGNC2Ensembl")
    HGNC2Ensembl(...)
}

#' @rdname deprecated
#' @export
initializeDirectory <- function(...) {
    # .Deprecated("initDir")
    initDir(...)
}

#' @rdname deprecated
#' @export
kables <- function(...) {
    # .Deprecated("markdownTables")
    markdownTables(...)
}

#' @rdname deprecated
#' @export
makeGene2symbolFromEnsembl <- function(...) {
    # .Deprecated("makeGene2SymbolFromEnsembl")
    do.call(
        what = makeGene2SymbolFromEnsembl,
        args = matchArgsToDoCall()
    )
}

#' @rdname deprecated
#' @export
makeGene2symbolFromGFF <- function(...) {
    # .Deprecated("makeGene2SymbolFromGFF")
    do.call(
        what = makeGene2SymbolFromGFF,
        args = matchArgsToDoCall()
    )
}

#' @rdname deprecated
#' @export
makeGene2symbolFromGTF <- function(...) {
    # .Deprecated("makeGene2SymbolFromGTF")
    do.call(
        what = makeGene2SymbolFromGTF,
        args = matchArgsToDoCall()
    )
}

#' @rdname deprecated
#' @export
makeTx2geneFromEnsembl <- function(...) {
    # .Deprecated("makeTx2GeneFromEnsembl")
    do.call(
        what = makeTx2GeneFromEnsembl,
        args = matchArgsToDoCall()
    )
}

#' @rdname deprecated
#' @export
makeTx2geneFromGFF <- function(...) {
    # .Deprecated("makeTx2GeneFromGFF")
    do.call(
        what = makeTx2GeneFromGFF,
        args = matchArgsToDoCall()
    )
}

#' @rdname deprecated
#' @export
makeTx2geneFromGTF <- function(...) {
    # .Deprecated("makeTx2GeneFromGTF")
    do.call(
        what = makeTx2GeneFromGTF,
        args = matchArgsToDoCall()
    )
}

#' @rdname deprecated
#' @export
mgi2gene <- function(...) {
    # .Deprecated("mgi2ensembl")
    mgi2ensembl(...)
}

#' @rdname deprecated
#' @export
mgi2ensembl <- function(...) {
    # .Deprecated("MGI2Ensembl")
    MGI2Ensembl(...)
}

#' @rdname deprecated
#' @export
panther <- function(...) {
    # .Deprecated("PANTHER")
    PANTHER(...)
}

#' @rdname deprecated
#' @export
readFileByExtension <- function(...) {
    # .Deprecated("import")
    import(...)
}

#' @rdname deprecated
#' @export
readGFF <- function(...) {
    # .Deprecated("import")
    import(...)
}

#' @rdname deprecated
#' @export
readGTF <- function(...) {
    # .Deprecated("import")
    import(...)
}

#' @rdname deprecated
#' @export
readJSON <- function(...) {
    # .Deprecated("import")
    import(...)
}

#' @rdname deprecated
#' @export
readYAML <- function(...) {
    # .Deprecated("import")
    import(...)
}

#' @rdname defunct
#' @export
setArgsToDoCall <- function(...) {
    .Defunct("matchArgsToDoCall")
}

# Soft deprecated until we update bcbio R packages.
#' @rdname deprecated
#' @export
tx2gene <- function(...) {
    # .Deprecated("Tx2Gene")
    Tx2Gene(...)
}



# v0.8.5 =======================================================================
#' @rdname deprecated
#' @export
markdownPlotlist <- function(plotlist, ...) {
    .Deprecated("markdownPlots")
    markdownPlots(list = plotlist, ...)
}

#' @rdname deprecated
#' @export
mdPlotlist <- markdownPlotlist



# v0.9.10 ======================================================================
# Legacy functions still in use by bcbio R packages that were previously defined
# in v0.7.2 release. Keep these functions soft deprecated, so a bunch of
# warnings don't pop up during assert checks in the older package versions.

# Source code is defined here primarily:
# https://github.com/steinbaugh/basejump/blob/v0.7.2/R/assert.R

# Imported by bcbioRNASeq 0.2.9.
#' @rdname deprecated
#' @export
assertAreGeneAnnotations <- function(x) {
    x <- as.data.frame(x)
    assert(
        isSubset(c("geneID", "geneName"), colnames(x)),
        hasRows(x)
    )
    TRUE
}

# Imported by bcbioRNASeq 0.2.9.
#' @rdname deprecated
#' @export
assertFormalGene2symbol <- function(x, genes, gene2symbol) {
    assert(
        hasRownames(x),
        isCharacter(genes, nullOK = TRUE),
        isAny(gene2symbol, classes = c("data.frame", "NULL"))
    )
    if (is.data.frame(gene2symbol)) {
        # Note that this function will be deprecated.
        assertIsGene2symbol(gene2symbol)
        assert(isSubset(rownames(x), gene2symbol[["geneID"]]))
    }
    TRUE
}

# Imported by bcbioBase 0.4.2.
#' @rdname deprecated
#' @export
assertFormalInterestingGroups <- function(x, interestingGroups) {
    # Early return on `NULL` interesting groups (e.g. DESeqDataSet).
    if (is.null(interestingGroups)) {
        return(TRUE)
    }

    assert(isCharacter(interestingGroups))

    # Obtain column data, if S4 object is passed in.
    if (isS4(x)) {
        x <- colData(x)
    }
    x <- as(x, "DataFrame")

    # Check that interesting groups are slotted into sampleData
    if (!isTRUE(isSubset(interestingGroups, colnames(x)))) {
        stop(paste(
            "The interesting groups",
            deparse(toString(setdiff(interestingGroups, colnames(x)))),
            "are not defined as columns in `sampleData()`."
        ))
    }

    # Check that interesting groups are factors
    isFactor <- vapply(
        X = x[, interestingGroups, drop = FALSE],
        FUN = is.factor,
        FUN.VALUE = logical(1L),
        USE.NAMES = TRUE
    )
    if (!all(isFactor)) {
        invalid <- names(isFactor)[which(!isFactor)]
        stop(paste(
            "The interesting groups",
            deparse(toString(invalid)),
            "are not factor."
        ))
    }

    TRUE
}

# Imported by bcbioRNASeq 0.2.9.
#' @rdname deprecated
#' @export
assertIsAHeaderLevel <- function(x) {
    assert(isHeaderLevel(x))
}

# Imported by bcbioRNASeq 0.2.9.
#' @rdname deprecated
#' @export
assertIsAStringOrNULL <- function(x) {
    assert(isString(x, nullOK = TRUE))
}

# Imported by bcbioBase 0.4.2.
#' @rdname deprecated
#' @export
assertIsAnImplicitInteger <- function(x) {
    assert(isInt(x, nullOK = FALSE))
}

# Imported by bcbioRNASeq 0.2.9.
#' @rdname deprecated
#' @export
assertIsAnImplicitIntegerOrNULL <- function(x) {
    assert(isInt(x, nullOK = TRUE))
}

# Imported by bcbioRNASeq 0.2.9.
#' @rdname deprecated
#' @export
assertIsColorScaleDiscreteOrNULL <- function(x) {
    assert(
        isGGScale(
            x = x,
            scale = "discrete",
            aes = "colour",
            nullOK = TRUE
        )
    )
}

# Imported by bcbioRNASeq 0.2.9.
#' @rdname deprecated
#' @export
assertIsFillScaleDiscreteOrNULL <- function(x) {
    assert(
        isGGScale(
            x = x,
            scale = "discrete",
            aes = "fill",
            nullOK = TRUE
        )
    )
}

# Imported by bcbioBase 0.4.2.
# Note that we don't want to check for `Gene2Symbol` S4 class here.
# Older versions of bcbio R packages don't slot gene-to-symbol mappings as our
# new and improved Gene2Symbol class.
#' @rdname deprecated
#' @export
assertIsGene2symbol <- function(x) {
    assert(
        is.data.frame(x),
        identical(colnames(x), c("geneID", "geneName")),
        hasRows(x),
        # Require that all columns are character.
        all(bapply(
            X = x,
            FUN = is.character
        ))
    )
}

# Imported by bcbioRNASeq 0.2.9.
#' @rdname deprecated
#' @export
assertIsHexColorFunctionOrNULL <- function(x) {
    assert(isHexColorFunction(x, nullOK = TRUE))
}

# Imported by bcbioRNASeq 0.2.9.
#' @rdname deprecated
#' @export
assertIsImplicitInteger <- function(x) {
    assert(isInt(x))
}

# Imported by bcbioBase 0.4.2.
# Note that we don't want to check for `Tx2Gene` S4 class here. Older versions
# of bcbio R packages don't slot transcript-to-gene mappings as our new and
# improved Tx2Gene class.
#' @rdname deprecated
#' @export
assertIsTx2gene <- function(x) {
    assert(is.data.frame(x))
    # Rename `txID` to `transcriptID`, if necessary.
    # Note that in newer code that uses `Tx2Gene` class, this is stricter.
    if ("txID" %in% colnames(x)) {
        colnames(x) <- gsub("^txID$", "transcriptID", colnames(x))
    }
    assert(
        identical(colnames(x), c("transcriptID", "geneID")),
        hasRows(x),
        # Require that all columns are character.
        all(bapply(
            X = x,
            FUN = is.character
        ))
    )
}



# nolint end
# nocov end
