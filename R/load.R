#' Data loading utilities
#'
#' @rdname load
#'
#' @param ... Data files as dot objects.
#' @param url URL.
#'
#' @examples
#' \dontrun{
#' loadData(geneIDs, oligo)
#' loadRemote("http://example.com/data.rda")
#' }
#'
#' @description Dynamically [load()] data from `data` directory or [source()]
#'   the corresponding script frmo the `data-raw` directory.
#' @export
loadData <- function(...) {
    envir <- parent.frame()
    names <- as.character(substitute(list(...)))[-1L]
    lapply(seq_along(names), function(a) {
        if (file.exists(str_c("data/", names[a], ".rda"))) {
            # Check for .rda file in `data/`
            message(paste("Loading", names[a], "from data"))
            load(str_c("data/", names[a], ".rda"), envir = envir)
        } else if (file.exists(str_c("data-raw/", names[a], ".rda"))) {
            # Check for .rda file in `data-raw/
            message(paste("Loading", names[a], "from data-raw"))
            load(str_c("data-raw/", names[a], ".rda"), envir = envir)
        } else if (file.exists(str_c("data-raw/", names[a], ".R"))) {
            # Source .R script in `data-raw/`
            message(paste("Sourcing", names[a], "from data-raw"))
            source(str_c("data-raw/", names[a], ".R"))
        } else {
            # Skip and warn
            warning(paste(names[a], "missing"))
        }
    }) %>%
        invisible
}



#' @rdname load
#' @description Load a remote R binary file.
#' @export
loadRemote <- function(url) {
    envir <- parent.frame()
    tempfile <- tempfile()
    download.file(url, get("tempfile"), quiet = TRUE)
    load(get("tempfile"), envir = envir)
}
