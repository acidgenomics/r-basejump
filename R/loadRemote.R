#' Load a remote R binary
#' @export
#' @importFrom utils download.file
#' @param url URL
#' @return Loaded R data file
loadRemote <- function(url) {
    tempfile <- tempfile()
    utils::download.file(url, get("tempfile"), quiet = TRUE)
    load(get("tempfile"), envir = globalenv())
}
