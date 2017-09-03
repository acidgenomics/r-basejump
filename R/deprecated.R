#' Deprecated Functions
#'
#' @rdname deprecated
#' @name deprecated
#' @keywords internal
#'
#' @inheritParams AllGenerics
#'
#' @return Deprecation warning.
NULL



#' @rdname deprecated
#' @export
packageSE <- function(...) {
    .Deprecated("prepareSummarizedExperiment")
    prepareSummarizedExperiment(...)
}



#' @rdname deprecated
#' @export
prepareSE <- function(...) {
    .Deprecated("prepareSummarizedExperiment")
    prepareSummarizedExperiment(...)
}
