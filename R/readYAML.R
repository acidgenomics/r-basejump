#' Read YAML File
#'
#' @family Read Functions
#'
#' @inheritParams general
#' @inheritParams saveData
#' @param object YAML file path.
#'
#' @return `list`.
#' @export
#'
#' @examples
#' yaml <- readYAML("http://basejump.seq.cloud/summary.yaml")
#' names(yaml)
readYAML <- function(object) {
    assert_is_a_string(object)
    assert_all_are_matching_regex(object, "\\.ya?ml$")
    file <- localOrRemoteFile(object)
    inform(paste("Reading", names(file)))
    yaml.load_file(file)
}
