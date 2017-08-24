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
    if (!str_detect(object, "\\.ya?ml$")) {
        stop("YAML file must have '.yaml' or '.yml' extension",
             call. = FALSE)
    }
    file <- .localOrRemoteFile(object)
    message(paste("Reading", names(file)))
    yaml.load_file(file)
})
