#' Load Local Data
#'
#' Dynamically [load()] data from `data/` directory or [source()] the
#' corresponding script from the `data-raw/` directory.
#'
#' @param dir Directory where the RData (`.rda`) files are located.
#' @param envir Environment where the objects will be loaded. Defaults to
#'   `parent.frame()`, also referred to as the calling environment. This
#'   argument supports new environment creation when a character string is used.
#'
#' @return No value.
#' @export
#'
#' @examples
#' \dontrun{
#' loadData(geneIDs, oligo)
#' loadData(geneIDs, envir = "newenv")
#' }
loadData <- function(..., dir = "data", envir = parent.frame()) {
    dots <- dots(..., character = TRUE)
    lapply(seq_along(dots), function(a) {
        file <- file.path(dir, paste0(dots[a], ".rda"))
        if (file.exists(file)) {
            message(paste("Loading", file, "from", dir))
            load(file, envir = envir)
        } else {
            # Skip and warn
            warning(paste(dots[a], "missing"))
        }
    }) %>%
        invisible
}
