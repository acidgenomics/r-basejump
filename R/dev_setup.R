#' Set up developer environment.
#'
#' This installs devtools, roxygen2, and checks RStudio environment.
#'
#' @return
#' @export
#'
#' @examples
dev_setup <- function() {
  install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
  install.packages("rstudioapi")

  devtools::install_github("hadley/devtools")
  library(devtools)
  has_devel()

  install.packages("lintr")
  lintr::lint_package()
}
