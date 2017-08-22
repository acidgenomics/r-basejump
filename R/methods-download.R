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
#'
#' @return No value.
#'
#' @examples
#' download("setup.R")
NULL



# Constructors ====
.download <- function(object, package = "basejump") {
    website <- get("website",
                   envir = as.environment(
                       paste("package", package, sep = ":")))
    sapply(seq_along(object), function(a) {
        if (!file.exists(object[[a]])) {
            download.file(
                file.path(website, "downloads", object[[a]]),
                destfile = object[[a]])
        }
    }) %>% invisible
}



# Methods ====
#' @rdname download
#' @export
setMethod("download", "missing", function(object, package = "basejump") {
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
