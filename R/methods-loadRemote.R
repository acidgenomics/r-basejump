#' Load Remote Data
#'
#' Load a remote R binary file.
#'
#' @rdname loadRemoteData
#'
#' @return No value.
#' @export
#'
#' @examples
#' \dontrun{
#' loadRemoteData("http://example.com/data.rda")
#' }
setMethod("loadRemoteData", "character", function(object) {
    envir <- parent.frame()
    tempfile <- tempfile()
    download.file(object, get("tempfile"), quiet = TRUE)
    load(get("tempfile"), envir = envir)
})
