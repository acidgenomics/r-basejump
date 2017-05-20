#' Data loading utilities
#'
#' @rdname load
#'
#' @description Dynamically [load()] data from `data` directory or [source()]
#'   the corresponding script frmo the `data-raw` directory.
#'
#' @param ... Data files as dot objects.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' loadData(geneIDs, oligo)
#' }
loadData <- function(data) {
    names <- as.character(substitute(list(...)))[-1L]
    sapply(seq_along(names), function(a) {
        if (file.exists(paste0("data/", names[a], ".rda"))) {
            # Check for .rda file in `data/`
            load(paste0("data/", names[a], ".rda"), envir = parent.frame())
        } else if (file.exists(paste0("data-raw/", names[a], ".rda"))) {
            # Check for .rda file in `data-raw/
            load(paste0("data-raw/", names[a], ".rda"), envir = parent.frame())
        } else if (file.exists(paste0("data-raw/", names[a], ".R"))) {
            # Source .R script in `data-raw/`
            source(paste0("data-raw/", names[a], ".R"))
        } else {
            # Skip and warn
            warning(paste(names[a], "missing"))
        }
    }) %>% invisible
}

#' @rdname aliases
#' @usage NULL
#' @export
load_data <- loadData



#' @rdname load
#' @description Load a remote R binary file.
#'
#' @param url URL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' loadRemote("http://example.com/data.rda")
#' }
loadRemote <- function(url) {
    tempfile <- tempfile()
    download.file(url, get("tempfile"), quiet = TRUE)
    load(get("tempfile"), envir = globalenv())
}

#' @rdname aliases
#' @usage NULL
#' @export
load_remote <- loadRemote
