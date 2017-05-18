#' Data loading utilities
#'
#' @rdname load
#'
#' @description Dynamically [source()] or [load()] raw data in the `data-raw`
#'   directory.
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

#' @rdname aliases
#' @usage NULL
#' @export
load_data_raw <- loadDataRaw



#' @rdname load
#' @description Load a remote R binary file.
#'
#' @param url URL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' loadRemote("http://example.com/data.rda")
#' }
loadRemote <- function(url) {
    tempfile <- tempfile()
    download.file(url, get("tempfile"), quiet = TRUE)
    load(get("tempfile"), envir = globalenv())
}

#' @rdname aliases
#' @usage NULL
#' @export
load_remote <- loadRemote
