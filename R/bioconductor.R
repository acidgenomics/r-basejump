#' Install or update Bioconductor packages
#'
#' This function is a convenience wrapper for [BiocInstaller::biocLite()]. It
#' sources the required `biocLite.R` file from the Bioconductor website
#' automatically then calls [biocLite()].
#'
#' @keywords internal
#'
#' @param pkgs Character vector of package names to install or update. Supports
#'   Bioconductor, CRAN, and GitHub repositories.
#' @param ... Passthrough parameters.
#'
#' @export
#'
#' @seealso This function is a modified version of `rafalib::install_bioc()`.
#'
#' @examples
#' \dontrun{
#' # Load Bioconductor and update packages
#' biocLite()
#'
#' # Install packages
#' biocLite("limma")            # Bioconductor
#' biocLite("dplyr")            # CRAN
#' biocLite("tidyverse/dplyr")  # GitHub
#' }
biocLite <- function(pkgs = NULL, ...) {
    internet <- try(
        source("https://bioconductor.org/biocLite.R"), silent = TRUE)
    if (!class(internet) == "try-error") {
        BiocInstaller::biocLite(pkgs, ...)
    } else {
        stop("Connection to bioconductor.org failed")
    }
}
