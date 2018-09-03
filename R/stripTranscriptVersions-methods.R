#' Strip Transcript Versions
#'
#' @note This function is strict, and will only strip Ensembl transcript IDs
#'   beginning with "ENS".
#'
#' @name stripTranscriptVersions
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return Object containing transcript identifiers without version numbers.
#'
#' @examples
#' # Ensembl
#' stripTranscriptVersions("ENSMUST00000000001.1")
#' stripTranscriptVersions("ENSMUST00000000001-1")
#' stripTranscriptVersions("ENSMUST00000000001_1")
#'
#' # WormBase (unmodified)
#'stripTranscriptVersions("cTel79B.1")
NULL



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("character"),
    definition = function(object) {
        # Pattern matching against Ensembl transcript IDs
        # http://www.ensembl.org/info/genome/stable_ids/index.html
        # Examples: ENST (human); ENSMUST (mouse)
        assert_is_character(object)
        assert_all_are_not_na(object)
        assert_all_are_non_missing_nor_empty_character(object)
        # punct will match `-` or `_` here
        gsub("^(ENS.*T\\d{11})[[:punct:]]\\d+$", "\\1", object)
    }
)



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("matrix"),
    definition = function(object) {
        rownames <- rownames(object)
        rownames <- stripTranscriptVersions(rownames)
        rownames(object) <- rownames
        object
    }
)



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("dgCMatrix"),
    definition = getMethod("stripTranscriptVersions", "matrix")
)
