#' Read YAML File
#'
#' @family Read Functions
#'
#' @importFrom yaml yaml.load_file
#'
#' @inheritParams general
#' @inheritParams saveData
#'
#' @param object YAML file path.
#'
#' @return `list`.
#' @export
#'
#' @examples
#' # bcbioRNASeq example project summary YAML
#' url <- paste(
#'     "http://bcbiobase.seq.cloud",
#'     "bcbio",
#'     "project-summary.yaml",
#'     sep = "/"
#' )
#' yaml <- readYAML(url)
#' names(yaml)
readYAML <- function(object) {
    assert_is_a_string(object)
    assert_all_are_matching_regex(object, "\\.ya?ml$")
    file <- localOrRemoteFile(object)
    inform(paste("Reading", names(file)))
    yaml.load_file(file)
}
