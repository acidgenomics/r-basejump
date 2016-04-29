#' Set up developer environment.
#'
#' This installs devtools, roxygen2, and checks RStudio environment.
#'
#' @return
#' @export
#'
#' @examples
dev_setup <- function() {
  library(devtools)
  has_devel()
  lintr::lint_package()
}
