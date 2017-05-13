#' Install or update Bioconductor packages.
#'
#' This function is a wrapper for [biocLite()]. It first sources the code from
#' the Bioconductor website then calls [biocLite()].
#'
#' @importFrom BiocCheck BiocCheck
#'
#' @param pkgs Character vector of package names to install or update. Supports
#'   Bioconductor, CRAN, and GitHub repositories (e.g.
#'   \code{steinbaugh/basejump} format).
#' @param ... Passthrough parameters.
#'
#' @export
#'
#' @seealso This function is a modified version of [rafalib::install_bioc()]. It
#'   calls [BiocInstaller::biocLite()] internally.
#'
#' @examples
#' \dontrun{
#' # Load Bioconductor and update packages
#' bioc()
#'
#' # Install `limma`
#' bioc("limma")
#' }
bioc <- function(pkgs = NULL, ...) {
    internet <- try(source("https://bioconductor.org/biocLite.R"),
                    silent = TRUE)
    if (!class(internet) == "try-error") {
        biocLite(pkgs, ...)
    } else {
        stop("No connection to bioconductor.org")
    }
}
