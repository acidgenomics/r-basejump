#' Strip Transcript Versions
#'
#' @rdname stripTranscriptVersions
#' @name stripTranscriptVersions
#' @author Michael Steinbaugh
#'
#' @inheritParams AllGenerics
#'
#' @return Transcript identifiers without version numbers.
#'
#' @examples
#' stripTranscriptVersions("ENSMUST00000000001.1")
NULL



# Constructors =================================================================
.stripTranscriptVersions <- function(object) {
    # Pattern matching against Ensembl transcript IDs
    # http://www.ensembl.org/info/genome/stable_ids/index.html
    # Examples: ENST (human); ENSMUST (mouse)
    assert_is_character(object)
    assert_all_are_not_na(object)
    assert_all_are_non_empty_character(object)
    assert_any_are_matching_regex(object, "^(ENS.*T\\d{11})\\.\\d+$")
    gsub("\\.\\d+$", "", object)
}



# Methods ======================================================================
#' @rdname stripTranscriptVersions
#' @export
setMethod(
    "stripTranscriptVersions",
    signature("character"),
    .stripTranscriptVersions)
