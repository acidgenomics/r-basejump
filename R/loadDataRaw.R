#' Load data-raw directory.
#'
#' Dynamically source or load raw data in the \code{data-raw} directory.
#'
#' @param data Data object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' loadDataRaw("counts")
#' }
loadDataRaw <- function(data) {
    for (a in 1:length(data)) {
        if (!file.exists(paste0("data/", data[a], ".rda"))) {
            source(paste0("data-raw/", data[a], ".R"))
        } else {
            load(paste0("data/", data[a], ".rda"), envir = parent.frame())
        }
    }
}



#' @rdname loadDataRaw
#' @usage NULL
#' @export
load_data_raw <- loadDataRaw
