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



## v0.10.4 =====================================================================
## Now recommending `plotCounts()` instead of `plotGene()`.
## Still in use by some revdeps, so keep re-exported.
#' @importFrom acidgenerics plotGene
#' @export
acidgenerics::plotGene



## v0.10.9 =====================================================================
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



## v0.11.11 ====================================================================
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



## v0.12.0 =====================================================================
## These were previously deprecated in transformer package.
#' @rdname defunct
#' @name flatFiles
#' @importFrom acidgenerics flatFiles
#' @export
NULL

#' @rdname deprecated
#' @export
coerceS4ToList <- function(...) {
    .Deprecated("coerceToList")
    coerceToList(...)
}

`flatFiles,SummarizedExperiment` <-  # nolint
    function(object) {
        .Defunct("coerceToList")
    }

#' @rdname defunct
#' @export
setMethod(
    f = "flatFiles",
    signature = signature("SummarizedExperiment"),
    definition = `flatFiles,SummarizedExperiment`
)



## v0.13.0 =====================================================================
#' @rdname deprecated
#' @export
matchEnsemblReleaseToURL <- function(...) {
    .Deprecated("mapEnsemblReleaseToURL")
    mapEnsemblReleaseToURL(...)
}



## nolint end
## nocov end
