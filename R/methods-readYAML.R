#' Read YAML File
#'
#' @rdname readYAML
#' @name readYAML
#' @family Data Import and Project Utilities
#'
#' @inheritParams AllGenerics
#' @param object YAML file path.
#'
#' @return [list].
#'
#' @examples
#' # bcbioRNASeq example project summary YAML
#' yamlFile <- file.path(
#'     "https://raw.githubusercontent.com",
#'     "hbc",
#'     "bcbioRNASeq",
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
    if (is.null(file)) {
        warning(paste(basename(object), "missing"), call. = FALSE)
        return(NULL)
    }
    message(paste("Reading", names(file)))
    yaml.load_file(file)
})
