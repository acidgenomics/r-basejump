#' Load Local Data
#'
#' Load RData (`.rda`) files from a directory using symbols rather than complete
#' file paths.
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
loadData <- function(..., dir = "data", envir = parent.frame()) {
    # The dots method will error at this step because the objects as symbols
    # aren't present in the calling environment.
    names <- as.character(substitute(list(...)))[-1L]
    message(paste("Loading", toString(names), "from", dir))
    files <- sapply(seq_along(names), function(a) {
        file <- file.path(dir, paste0(names[a], ".rda"))
        if (file.exists(file)) {
            file <- normalizePath(file)
            name <- load(file, envir = envir)
            names(file) <- name
            file
        } else {
            # Skip and warn
            warning(paste(names[a], "missing"), call. = FALSE)
        }
    })
    invisible(files)
}
