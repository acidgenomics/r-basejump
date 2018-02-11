#' Read YAML File
#'
#' @rdname readYAML
#' @name readYAML
#' @family Data Import and Project Utilities
#'
#' @inheritParams general
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



# Constructors =================================================================
.readYAML <- function(
    object,
    quiet = FALSE) {
    assert_is_a_string(object)
    assert_all_are_matching_regex(object, "\\.ya?ml$")
    assert_is_a_bool(quiet)
    file <- localOrRemoteFile(object, quiet = quiet)
    if (!isTRUE(quiet)) {
        inform(paste("Reading", names(file)))
    }
    yaml.load_file(file)
}


# Methods ======================================================================
#' @rdname readYAML
#' @importFrom yaml yaml.load_file
#' @export
setMethod(
    "readYAML",
    signature("character"),
    .readYAML)
