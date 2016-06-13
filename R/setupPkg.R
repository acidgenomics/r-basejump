#' Setup R packages
#'
#' This function allows for dynamic install and loading of packages from CRAN,
#' Bioconductor, and GitHub.
#'
#' @param pkg Character vector of package names
#' @param source Where to download packages (CRAN, Bioconductor, GitHub)
#'
#' @return Installs packages if necessary, then loads into current workspace.
#' @export
#'
#' @examples
#' setupPkg(c("plyr", "readr"), source = "cran")
setupPkg <- function(pkg, source = "cran") {
  #! This needs a rework -- use a for loop?
  pkgName <- pkg
  if (source == "github") {
    pkgName <- gsub("^.*/", "", pkg)
  }
  pkgNeedsInstall <- pkgName[!(pkgName %in% installed.packages()[, "Package"])]
  if (source == "cran" & length(pkgNeedsInstall) > 0) {
    install.packages(pkgNeedsInstall, repos = "http://cran.rstudio.com/")
  }
  if (source == "bioc" & length(pkgNeedsInstall) > 0) {
    biocLite <- NULL; rm(biocLite)
    source("https://bioconductor.org/biocLite.R")
    biocLite(pkgNeedsInstall)
  }
  if (source == "github" & length(pkgNeedsInstall) > 0) {
    # This step doesn't right because it needs to call full package name
    devtools::install_github(pkgNeedsInstall)
  }
  invisible(lapply(pkgName, require, character.only = TRUE))
}
