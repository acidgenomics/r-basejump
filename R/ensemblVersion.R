#' Ensembl Build Version
#'
#' @keywords internal
#'
#' @return String containing Ensembl build version of annotables.
#' @export
#'
#' @examples
#' ensemblVersion()
ensemblVersion <- function() {
    annotables::ensembl_version
}
