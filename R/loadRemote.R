#' Load a remote R binary file
#'
#' @author Michael Steinbaugh
#'
#' @importFrom utils download.file
#'
#' @param url URL
#'
#' @export
#'
#' @examples
#' \dontrun{
#' loadRemote("http://example.com/data.rda")
#' }
loadRemote <- function(url) {
    tempfile <- tempfile()
    utils::download.file(url, get("tempfile"), quiet = TRUE)
    load(get("tempfile"), envir = globalenv())
}
