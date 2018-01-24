#' Read YAML File
#'
#' @rdname readYAML
#' @name readYAML
#' @family Data Import and Project Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams saveData
#'
#' @param object YAML file path.
#'
#' @return [list].
#'
#' @examples
#' # bcbioRNASeq example project summary YAML
#' url <- file.path(
#'     "http://bcbiobase.seq.cloud",
#'     "bcbio",
#'     "project-summary.yaml")
#' yaml <- readYAML(url)
#' names(yaml)
NULL



# Methods ======================================================================
#' @rdname readYAML
#' @importFrom yaml yaml.load_file
#' @export
setMethod(
    "readYAML",
    signature("character"),
    function(
        object,
        quiet = FALSE) {
        if (!grepl(x = object, pattern = "\\.ya?ml$")) {
            abort("YAML file must contain `.yaml` or `.yml` extension")
        }
        file <- localOrRemoteFile(object, quiet = quiet)
        if (is.null(file)) {
            warn(paste(
                basename(object), "file missing"
            ))
            return(NULL)
        }
        if (!isTRUE(quiet)) {
            inform(paste("Reading", names(file)))
        }
        yaml.load_file(file)
    })
