#' Load Local Data
#'
#' Load RData (`.rda`) files from a directory using symbols rather than complete
#' file paths.
#'
#' @param dir Directory where the RData (`.rda`) files are located.
#'
#' @return No value.
#' @export
#'
#' @examples
#' \dontrun{
#' loadData(geneIDs, oligo)
#' }
loadData <- function(..., dir = "data") {
    envir <- parent.frame()
    names <- as.character(substitute(list(...)))[-1L]
    message(paste("Loading", toString(names), "from", dir))
    lapply(seq_along(names), function(a) {
        file <- file.path(dir, paste0(names[a], ".rda"))
        if (file.exists(file)) {
            load(file, envir = envir)
        } else {
            # Skip and warn
            warning(paste(names[a], "missing"), call. = FALSE)
        }
    }) %>%
        invisible
}
