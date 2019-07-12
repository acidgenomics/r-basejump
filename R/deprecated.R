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



# v0.8.0 =======================================================================
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
    # .Deprecated("sanitizeNA")  # nolint
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

#' @rdname deprecated
#' @export
readFileByExtension <- function(...) {
    # .Deprecated("import")  # nolint
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
    # .Deprecated("import")  # nolint
    import(...)
}

#' @rdname defunct
#' @export
setArgsToDoCall <- function(...) {
    .Defunct("matchArgsToDoCall")
}

# Soft deprecated until we update bcbio R packages.
#' @rdname defunct
#' @export
tx2gene <- function(...) {
    .Defunct("Tx2Gene")
}



# v0.8.5 =======================================================================
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
        # [2019-02-17] Note that we've added `Gene2Symbol` here.
        isAny(gene2symbol, classes = c("Gene2Symbol", "data.frame", "NULL"))
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



# v0.10.4 ======================================================================
# Now recommending `plotCounts()` instead of `plotGene()`.
# Still in use by some revdeps, so keep re-exported.
#' @importFrom bioverbs plotGene
#' @export
bioverbs::plotGene



# v0.10.9 ======================================================================
#' @rdname defunct
#' @export
assertHasRownames <- function(...) {
    .Defunct(msg = paste(
        "'assertHasRownames' is defunct.",
        "Use 'assert(hasRownames())' in goalie package instead.",
        "See help(\"Defunct\")",
        sep = "\n"
    ))
}

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



# nolint end
# nocov end
