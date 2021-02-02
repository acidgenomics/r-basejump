## nocov start
## nolint start



#' @name defunct
#' @inherit AcidRoxygen::defunct description examples return seealso title
#' @inheritParams AcidRoxygen::params
#' @keywords internal
NULL



## #' @name deprecated
## #' @inherit AcidRoxygen::deprecated description examples return seealso title
## #' @inheritParams AcidRoxygen::params
## #' @keywords internal
## NULL



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
#' @rdname defunct
#' @export
readFileByExtension <- function(...) {
    .Defunct("import")
}

#' @rdname defunct
#' @export
readGFF <- function(...) {
    .Defunct("import")
}

#' @rdname defunct
#' @export
readGTF <- function(...) {
    .Defunct("import")
}

#' @rdname defunct
#' @export
readJSON <- function(...) {
    .Defunct("import")
}

#' @rdname defunct
#' @export
readYAML <- function(...) {
    .Defunct("import")
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
#' @rdname defunct
#' @export
readSampleData <- function(...) {
    .Defunct("importSampleData")
}

#' @rdname defunct
#' @export
readTx2Gene <- function(...) {
    .Defunct("importTx2Gene")
}



## v0.12.0 =====================================================================
#' @rdname defunct
#' @export
coerceS4ToList <- function(...) {
    .Defunct("coerceToList")
}



## v0.13.0 =====================================================================
#' @rdname defunct
#' @export
matchEnsemblReleaseToURL <- function(...) {
    .Defunct("mapEnsemblReleaseToURL")
}

#' @rdname defunct
#' @export
matchHumanOrthologs <- function(...) {
    .Defunct("mapHumanOrthologs")
}



## v0.14.0 =====================================================================
#' @rdname defunct
#' @export
multiassignAsEnvir <- function(...) {
    .Defunct()
}

#' @rdname defunct
#' @export
sortUnique <- function(...) {
    .Defunct()
}

#' @rdname defunct
#' @export
toStringUnique <- function(...) {
    .Defunct()
}



## nolint end
## nocov end
