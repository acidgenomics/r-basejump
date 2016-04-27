#' Manage R packages.
#'
#' This function allows for dynamic install and loading of packages from CRAN,
#' Bioconductor, and GitHub.
#'
#' @param pkg
#' @param source
#'
#' @return
#' @export
#'
#' @examples
manage_pkg <- function(pkg, source = "cran") {
  install_pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (source == "cran" & length(install_pkg) > 0) {
    install.packages(install_pkg)
  }
  if (source == "bioc" & length(install_pkg) > 0) {
    source("https://bioconductor.org/biocLite.R")
    biocLite(install_pkg)
  }
  if (source == "github" & length(install_pkg) > 0) {
    library(devtools)
    devtools::install_github(install_github_pkg)
  }
  invisible(lapply(pkg, require, character.only = TRUE))
}
