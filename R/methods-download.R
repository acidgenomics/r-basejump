#' Download Dependency File
#'
#' If the required dependency file isn't present, download latest version from
#' the package website.
#'
#' File download utility for RMarkdown knit reports.
#'
#' @rdname download
#' @name download
#'
#' @param object *Optional*. File name. If `NULL` (default), download the
#'   default dependency files for a new experiment.
#' @param package Package name.
#'
#' @return No value.
#'
#' @examples
#' \dontrun{
#' download("setup.R", package = "bcbioSinglecell")
#' }
NULL



# Constructors ====
.download <- function(object, package) {
    envir <- tryCatch(
        loadNamespace(package),
        error = function(a) {
            stop(paste(package, "package not found"), call. = FALSE)
        })
    if (!"url" %in% names(envir)) {
        stop(paste(package, "package doesn't export required 'url'"),
             call. = FALSE)
    }
    url <- get("url", envir = envir)
    sapply(seq_along(object), function(a) {
        if (!file.exists(object[[a]])) {
            download.file(
                file.path(url, "downloads", object[[a]]),
                destfile = object[[a]])
        }
    }) %>%
        invisible
}



# Methods ====
#' @rdname download
#' @export
setMethod("download", "missing", function(object, package) {
    .download(
        c("_output.yaml",
          "_footer.Rmd",
          "_header.Rmd",
          paste0(package, ".bib"),
          "setup.R"))
})



#' @rdname download
#' @export
setMethod("download", "character", .download)
