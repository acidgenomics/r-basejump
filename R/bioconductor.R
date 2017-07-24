#' Install or Update Bioconductor Packages
#'
#' This function is a convenience wrapper for [BiocInstaller::biocLite()]. It
#' sources the required [biocLite.R](https://bioconductor.org/biocLite.R) file
#' from the Bioconductor website automatically then calls [biocLite()].
#'
#' Supports [Bioconductor](https://bioconductor.org),
#' [CRAN](https://cran.r-project.org), and [GitHub](https://github.com)
#' repositories.
#'
#' @keywords internal
#'
#' @param pkgs Character vector of package names.
#' @param ... Additional arguments, passed to [BiocInstaller::biocLite()].
#'
#' @export
#'
#' @seealso Modified variant of `rafalib::install_bioc()`.
#'
#' @examples
#' \dontrun{
#' # Update all packages
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
