#' Initialize package in development.
#'
#' @export
sourceAll <- function() {
  devtools::load_all()
  sapply(list.files(path = "R", pattern = "*.R", full.names = TRUE), source)
}
