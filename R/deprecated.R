## nocov start
## nolint start



#' @name defunct
#' @inherit AcidRoxygen::defunct description examples return seealso title
#' @inheritParams AcidRoxygen::params
#' @keywords internal
NULL



#' @name deprecated
#' @inherit AcidRoxygen::deprecated description examples return seealso title
#' @inheritParams AcidRoxygen::params
#' @keywords internal
NULL



## v0.11.11 ====================================================================
## NOTE This is still called by bcbioBase, need to update namespace.
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
matchArgsToDoCall <- function(...) {
    .Defunct()
}

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
