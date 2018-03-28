#' Read YAML File
#'
#' @family Read Functions
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
    assert_is_a_string(file)
    assert_all_are_matching_regex(file, "\\.ya?ml$")
    file <- localOrRemoteFile(file)
    inform(paste("Reading", names(file)))
    yaml.load_file(file)
}
