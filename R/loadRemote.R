#' Load a remote R binary data file
#'
#' @importFrom utils download.file
#'
#' @param url URL
#' @param ... Passthrough to `load()` function.
#'
#' @return Loaded R data file
#' @export
loadRemote <- function(url, ...) {
    tempfile <- tempfile()
    utils::download.file(url, get("tempfile"), quiet = TRUE)
    load(get("tempfile"), ...)
}
