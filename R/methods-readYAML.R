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
#' yaml <- readYAML("http://basejump.seq.cloud/project-summary.yaml")
#' names(yaml)
NULL



# Methods ====
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
            stop("YAML file must have '.yaml' or '.yml' extension",
                 call. = FALSE)
        }
        file <- .localOrRemoteFile(object, quiet = quiet)
        if (is.null(file)) {
            warning(paste(
                basename(object), "file missing"
            ), call. = FALSE)
            return(NULL)
        }
        if (!isTRUE(quiet)) {
            message(paste("Reading", names(file)))
        }
        yaml.load_file(file)
    })
