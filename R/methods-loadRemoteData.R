#' Load Remote Data
#'
#' Load a remote R binary file.
#'
#' @rdname loadRemoteData
#' @name loadRemoteData
#'
#' @return No value.
#'
#' @examples
#' loadRemoteData("http://steinbaugh.com/basejump/tests/mtcars.rda")
NULL



# Methods ====
#' @rdname loadRemoteData
#' @export
setMethod("loadRemoteData", "character", function(object) {
    envir <- parent.frame()
    tempfile <- tempfile()
    download.file(object, get("tempfile"), quiet = TRUE)
    get("tempfile") %>%
        load(envir = envir)
})
