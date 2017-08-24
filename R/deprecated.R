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
    .Deprecated("prepareSE")
    prepareSE(...)
}
