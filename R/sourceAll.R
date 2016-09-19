#' Initialize package in development.
#'
#' @import devtools
#'
#' @export
sourceAll <- function() {
    devtools::load_all()
    sapply(list.files(path = "R", pattern = "*.R", full.names = TRUE), source)
    if (file.exists("R/sysdata.rda")) {
        load("R/sysdata.rda", .GlobalEnv)
    }
}
