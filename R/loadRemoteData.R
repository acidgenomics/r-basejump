#' Load Remote Data
#'
#' Load a remote R binary file.
#'
#' @family Data Import and Project Utilities
#'
#' @importFrom rlang is_string
#' @importFrom utils download.file
#'
#' @inheritParams AllGenerics
#' @inheritParams loadData
#' @inheritParams readFileByExtension
#'
#' @return Silently return loaded object name.
#' @export
#'
#' @examples
#' loadRemoteData(file.path(testDataURL, "mtcars.rda"))
loadRemoteData <- function(
    object,
    envir = parent.frame(),
    quiet = FALSE) {
    if (!is_string(object)) {
        stop("'object' must be a string", call. = FALSE)
    }
    # Check for remote URL
    if (!grepl(x = object, pattern = "\\://")) {
        stop("Remote URL containing '://' required", call. = FALSE)
    }
    # Check for '.rda' file
    if (!grepl(x = object, pattern = "\\.rda$")) {
        stop("Data file must contain '.rda' extension", call. = FALSE)
    }
    if (!is.environment(envir)) {
        stop("'envir' must be an environment", call. = FALSE)
    }
    tmp <- tempfile()
    download.file(
        url = object,
        destfile = tmp,
        quiet = quiet)
    return <- load(file = tmp, envir = envir)
    invisible(return)
}
