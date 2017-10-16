#' Load Local Data
#'
#' Load RData (`.rda`) files from a directory using symbols rather than complete
#' file paths.
#'
#' @importFrom rlang is_string
#'
#' @inheritParams saveData
#' @param envir Environment to use for assignment. Defaults to `parent.frame()`,
#' which will assign into the calling environment.
#'
#' @return Silent named character vector of file paths.
#' @export
#'
#' @examples
#' \dontrun{
#' loadData(geneIDs, oligo)
#' }
loadData <- function(
    ...,
    dir = "data",
    envir = parent.frame()) {
    if (!is_string(dir)) {
        stop("'dir' must be a string", call. = FALSE)
    }
    if (!is.environment(envir)) {
        stop("'envir' must be an environment", call. = FALSE)
    }
    # The dots method will error at this step because the objects (as symbols)
    # aren't present in the calling environment
    names <- as.character(substitute(list(...)))[-1L]
    message(paste("Loading", toString(names), "from", dir))
    files <- sapply(seq_along(names), function(a) {
        name <- names[a]
        file <- file.path(dir, paste0(name, ".rda"))
        if (!file.exists(file)) {
            stop(paste(name, "missing"), call. = FALSE)
        }
        file <- normalizePath(file)
        loaded <- load(file, envir = envir)
        if (!identical(name, loaded)) {
            stop(paste(
                name, "file and saved object names are not identical"
            ), call. = FALSE)
        }
        names(file) <- name
        file
    })
    invisible(files)
}
