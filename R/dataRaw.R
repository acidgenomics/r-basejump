#' Load raw data
#'
#' Dynamically source or load raw data in the `data-raw` directory
#'
#' @author Michael Steinbaugh
#'
#' @keywords internal
#'
#' @param data Data object
#'
#' @export
dataRaw <- function(data) {
    for (a in 1:length(data)) {
        if (!file.exists(paste0("data/", data[a], ".rda"))) {
            source(paste0("data-raw/", data[a], ".R"))
        } else {
            load(paste0("data/", data[a], ".rda"), envir = globalenv())
        }
    }
}
