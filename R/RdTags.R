#' R Documentation Tags
#'
#' Modified version of the unexported `tools:::RdTags()` function.
#'
#' @family Documentation Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams parseRd
#'
#' @examples
#' db <- tools::Rd_db("base")
#' Rd <- db[["nrow.Rd"]]
#' RdTags(Rd)
RdTags <- function(object) {  # nolint
    assert_is_all_of(object, "Rd")
    tags <- sapply(object, attr, "Rd_tag")
    if (!has_length(tags)) {
        tags <- character()
    } else {
        # Remove the leading "\\" backslashes.
        tags <- gsub("^\\\\", "", tags)
    }
    tags
}
