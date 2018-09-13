#' Read JSON File
#'
#' @family Read Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `list`.
#'
#' @examples
#' x <- readJSON("http://basejump.seq.cloud/example.json")
#' names(x)
readJSON <- function(file) {
    assert_is_a_string(file)
    assert_all_are_matching_regex(file, "\\.json$")
    message(paste("Reading", basename(file)))
    file <- localOrRemoteFile(file)
    read_json(file)
}
