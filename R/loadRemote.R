#' Load a remote R binary file.
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



#' @rdname loadRemote
#' @usage NULL
#' @export
load_remote <- loadRemote
