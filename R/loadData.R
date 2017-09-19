#' Load Local Data
#'
#' Dynamically [load()] data from `data/` directory or [source()] the
#' corresponding script from the `data-raw/` directory.
#'
#' @param dir Directory where the RData (`.rda`) files are located.
#'
#' @return No value.
#' @export
#'
#' @examples
#' \dontrun{
#' loadData(geneIDs, oligo)
#' loadData(geneIDs, envir = "newenv")
#' }
loadData <- function(..., dir = "data") {
    envir <- parent.frame()
    names <- dots(..., character = TRUE)
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
