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
  pkg_name <- pkg
  # Rename GitHub package input
  if (source == "github") {
    pkg_name <- gsub("^.*/", "", pkg)
  }
  install_pkg <- pkg_name[!(pkg_name %in% installed.packages()[, "Package"])]
  if (source == "cran" & length(install_pkg) > 0) {
    install.packages(install_pkg)
  }
  if (source == "bioc" & length(install_pkg) > 0) {
    source("https://bioconductor.org/biocLite.R")
    biocLite(install_pkg)
  }
  if (source == "github" & length(install_pkg) > 0) {
    library(devtools)
    devtools::install_github(install_pkg)
  }
  invisible(lapply(pkg_name, require, character.only = TRUE))
}
