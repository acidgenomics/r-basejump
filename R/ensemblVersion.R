#' Ensembl Build Version
#'
#' @rdname ensemblVersion
#' @name ensemblVersion
#'
#' @keywords internal
#'
#' @return String containing Ensembl build version of annotables.
#' @export
#'
#' @examples
#' ensemblVersion()
NULL



# Methods ====
#' @rdname ensemblVersion
#' @export
setMethod("ensemblVersion", "missing", function() {
    annotables::ensembl_version
})
