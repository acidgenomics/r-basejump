#' Read YAML File
#'
#' @family Read Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `list`.
#' @export
#'
#' @examples
#' yaml <- readYAML("http://basejump.seq.cloud/summary.yaml")
#' names(yaml)
readYAML <- function(file) {
    requireNamespace("yaml")
    assert_is_a_string(file)
    assert_all_are_matching_regex(file, "\\.ya?ml$")
    file <- localOrRemoteFile(file)
    message(paste("Reading", names(file)))
    yaml::yaml.load_file(file)
}
