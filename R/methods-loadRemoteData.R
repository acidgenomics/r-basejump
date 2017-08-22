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
    # Check for remote URL
    if (!str_detect(object, "\\://")) {
        stop("Remote URL containing '://' required")
    }
    # Check for '.rda' file
    if (!str_detect(object, "\\.rda$")) {
        stop("Data file must contain '.rda' extension")
    }
    envir <- parent.frame()
    tempfile <- tempfile()
    download.file(object, get("tempfile"), quiet = TRUE)
    get("tempfile") %>%
        load(envir = envir)
})
