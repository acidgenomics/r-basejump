#' Load Remote Data
#'
#' Load a remote R binary file.
#'
#' @rdname load
#' @export
#'
#' @examples
#' \dontrun{
#' loadRemoteData("http://example.com/data.rda")
#' }
setMethod("loadRemote", "character", function(object) {
    envir <- parent.frame()
    tempfile <- tempfile()
    download.file(object, get("tempfile"), quiet = TRUE)
    load(get("tempfile"), envir = envir)
})
