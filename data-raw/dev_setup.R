#' Set up developer environment
#'
#' Installs devtools, roxygen2, and lintr.
#'
#' @return Installs devtools, roxygen2, and lintr.
#' @export
#'
#' @examples
#' dev_setup()
dev_setup <- function() {
  # devtools
  install.packages("devtools")
  library(devtools)
  has_devel()

  # roxygen2
  install.packages("roxygen2")
  library(roxygen2)

  # lintr
  install.packages("lintr")
  library(lintr)
  lintr::lint_package()
}
