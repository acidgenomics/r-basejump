#' Read YAML File
#'
#' @rdname readYAML
#' @name readYAML
#'
#' @param object YAML file path.
#'
#' @return [list].
#'
#' @examples
#' # bcbioRnaseq example project summary YAML
#' yamlFile <- file.path(
#'     "https://raw.githubusercontent.com",
#'     "hbc",
#'     "bcbioRnaseq",
#'     "master",
#'     "inst",
#'     "extra",
#'     "bcbio",
#'     "2017-05-23_rnaseq",
#'     "project-summary.yaml")
#' yaml <- readYAML(yamlFile)
#' names(yaml)
NULL



# Methods ====
#' @rdname readYAML
#' @export
setMethod("readYAML", "character", function(object) {
    if (str_detect(object, "\\://")) {
        # Remote file
        filePath <- object
        tempfile <- tempfile()
        download.file(object, tempfile)
    } else {
        # Local file
        filePath <- normalizePath(object)
    }
    fileName <- basename(filePath)
    message(paste("Reading", fileName))
    yaml.load_file(filePath)
})
