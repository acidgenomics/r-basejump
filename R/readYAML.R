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
#' x <- readYAML("http://basejump.seq.cloud/example.yml")
#' names(x)
readYAML <- function(file) {
    requireNamespace("yaml")
    assert_is_a_string(file)
    assert_all_are_matching_regex(file, "\\.ya?ml$")
    message(paste("Reading", basename(file)))
    file <- localOrRemoteFile(file)
    yaml::yaml.load_file(file)
}
