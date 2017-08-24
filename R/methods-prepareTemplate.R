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
#' @param sourceDir Source directory, typically a URL, where the dependency
#'   files are located.
#'
#' @return No value.
#'
#' @examples
#' \dontrun{
#' sourceDir <- "http://bioinformatics.sph.harvard.edu/bcbioRnaseq/downloads"
#' prepareTemplate(sourceDir)
#' prepareTemplate("setup.R", sourceDir)
#' }
NULL



# Constructors ====
.downloadPackageFile <- function(object, sourceDir) {
    sapply(seq_along(object), function(a) {
        if (!file.exists(object[[a]])) {
            download.file(
                file.path(sourceDir, object[[a]]),
                destfile = object[[a]])
        }
    }) %>%
        invisible
}



# Methods ====
#' @rdname prepareTemplate
#' @export
setMethod("prepareTemplate", "missing", function(object, sourceDir) {
    .downloadPackageFile(
        c("_output.yaml",
          "_footer.Rmd",
          "_header.Rmd",
          "bibliography.bib",
          "setup.R"),
        sourceDir = sourceDir)
})



#' @rdname prepareTemplate
#' @export
setMethod("prepareTemplate", "character", .downloadPackageFile)
