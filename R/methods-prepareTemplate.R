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
#' @param overwrite Overwrite files if they already exist. *This option is used
#'   primarily for unit testing, and is not generally recommended.*
#'
#' @return No value.
#'
#' @examples
#' \dontrun{
#' # Load the shared files from basejump
#' prepareTemplate()
#'
#' # Request individual files
#' prepareTemplate(c("setup.R", "_header.Rmd"))
#'
#' # Load the shared files from bcbioSingleCell
#' prepareTemplate(
#'     sourceDir = system.file("rmarkdown/shared",
#'                             package = "bcbioSingleCell"))
#' }
NULL



# Constructors ====
.copyPackageFile <- function(object, sourceDir, overwrite = FALSE) {
    if (missing(sourceDir)) {
        sourceDir <- system.file("rmarkdown/shared", package = "basejump")
    }
    if (isTRUE(overwrite)) {
        message(paste("Overwriting", toString(object)))
    }
    sapply(seq_along(object), function(a) {
        if (!file.exists(object[[a]])) {
            file.copy(
                from = file.path(sourceDir, object[[a]]),
                to = object[[a]],
                overwrite = overwrite)
        }
    }) %>%
        invisible()
}



# Methods ====
#' @rdname prepareTemplate
#' @export
setMethod("prepareTemplate", "missing", function(
    object,
    sourceDir) {
    .copyPackageFile(
        c("_output.yaml",
          "_footer.Rmd",
          "_header.Rmd",
          "bibliography.bib",
          "setup.R"),
        sourceDir = sourceDir,
        overwrite = FALSE)
})



#' @rdname prepareTemplate
#' @export
setMethod("prepareTemplate", "character", .copyPackageFile)
