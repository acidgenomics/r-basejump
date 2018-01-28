#' Load Remote Data
#'
#' Load a remote R binary file.
#'
#' @family Data Import and Project Utilities
#'
#' @importFrom utils download.file
#'
#' @inheritParams AllGenerics
#' @inheritParams loadData
#'
#' @return Silently return loaded object name.
#' @export
#'
#' @examples
#' loadRemoteData("http://basejump.seq.cloud/mtcars.rda")
loadRemoteData <- function(
    object,
    envir = parent.frame(),
    quiet = FALSE) {
    if (!is_string(object)) {
        abort("`object` must be a string")
    }
    # Check for remote URL
    if (!grepl("\\://", object)) {
        abort("Remote URL containing `://` required")
    }
    # Check for `.rda` file
    if (!grepl("\\.rda$", object)) {
        abort("Data file must contain `.rda` extension")
    }
    if (!is.environment(envir)) {
        abort("`envir` must be an environment")
    }
    tmp <- tempfile()
    download.file(
        url = object,
        destfile = tmp,
        quiet = quiet)
    return <- load(file = tmp, envir = envir)
    invisible(return)
}
