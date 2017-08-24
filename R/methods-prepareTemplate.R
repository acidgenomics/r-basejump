#' Download External Dependency File
#'
#' If the required template dependency files aren't present, download latest
#' versions from the package website.
#'
#' File download utility for RMarkdown knit reports.
#'
#' @rdname prepareTemplate
#' @name prepareTemplate
#'
#' @param object *Optional*. File name. If `NULL` (default), download the
#'   default dependency files for a new experiment.
#' @param package Package name.
#'
#' @return No value.
#'
#' @examples
#' \dontrun{
#' prepareTemplate(package = "bcbioSinglecell")
#' prepareTemplate("setup.R", package = "bcbioSinglecell")
#' }
NULL



# Constructors ====
.downloadPackageFile <- function(object, package) {
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
#' @rdname prepareTemplate
#' @export
setMethod("prepareTemplate", "missing", function(object, package) {
    .downloadPackageFile(
        c("_output.yaml",
          "_footer.Rmd",
          "_header.Rmd",
          "bibliography.bib",
          "setup.R"))
})



#' @rdname prepareTemplate
#' @export
setMethod("prepareTemplate", "character", .downloadPackageFile)
