#' Load Remote Data
#'
#' Load a remote R binary file.
#'
#' @rdname loadRemoteData
#' @name loadRemoteData
#' @family Data Import and Project Utilities
#'
#' @inheritParams AllGenerics
#'
#' @return No value.
#'
#' @examples
#' loadRemoteData(file.path(testDataURL, "mtcars.rda"))
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
    tmp <- tempfile()
    download.file(object, tmp)
    load(tmp, envir = envir)
})
