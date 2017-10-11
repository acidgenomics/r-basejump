#' Load Remote Data
#'
#' Load a remote R binary file.
#'
#' @family Data Import and Project Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams loadData
#'
#' @return No value.
#'
#' @examples
#' loadRemoteData(file.path(testDataURL, "mtcars.rda"))
loadRemoteData <- function(object, envir = parent.frame()) {
    if (!is_string(object)) {
        stop("'object' must be a string", call. = FALSE)
    }
    # Check for remote URL
    if (!str_detect(object, "\\://")) {
        stop("Remote URL containing '://' required", call. = FALSE)
    }
    # Check for '.rda' file
    if (!str_detect(object, "\\.rda$")) {
        stop("Data file must contain '.rda' extension", call. = FALSE)
    }
    if (!is.environment(envir)) {
        stop("'envir' must be an environment", call. = FALSE)
    }
    tmp <- tempfile()
    download.file(object, tmp)
    load(tmp, envir = envir)
}
