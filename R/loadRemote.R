#' Load a remote R binary
#'
#' @author Michael Steinbaugh
#' @keywords data package
#'
#' @importFrom utils download.file
#'
#' @param url URL
#'
#' @return Loaded R data file
#' @export
loadRemote <- function(url) {
    tempfile <- tempfile()
    utils::download.file(url, get("tempfile"), quiet = TRUE)
    load(get("tempfile"), envir = globalenv())
}
