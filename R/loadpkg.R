#' Initialize package in development
#'
#' @examples
#' loadpkg()
#' @export
loadpkg <- function() {
  if (file.exists("R")) {
    sapply(
      list.files(path = "R", pattern = "*.R", full.names = TRUE),
      source
    )
  }
  if (file.exists("R/sysdata.rda")) {
    load("R/sysdata.rda", .GlobalEnv)
  }
  if (file.exists("data")) {
    sapply(
      list.files(path = "data", pattern = "*.rda", full.names = TRUE),
      load, .GlobalEnv
    )
  }
}
