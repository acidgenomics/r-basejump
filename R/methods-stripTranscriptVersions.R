#' Strip Transcript Versions
#'
#' @note This function is strict, and will only strip Ensembl transcript IDs
#'   beginning with "ENS".
#'
#' @name stripTranscriptVersions
#' @family Gene Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return Transcript identifiers without version numbers.
#'
#' @examples
#' # Ensembl
#' stripTranscriptVersions("ENSMUST00000000001.1")
#'
#' # WormBase (unmodified)
#'stripTranscriptVersions("cTel79B.1")
NULL



# Constructors =================================================================
.stripTranscriptsVersions.dim <- function(object) {  # nolint
    x <- rownames(object)
    x <- stripTranscriptVersions(x)
    rownames(object) <- x
    object
}



# Methods ======================================================================
#' @rdname stripTranscriptVersions
#' @export
setMethod(
    "stripTranscriptVersions",
    signature("character"),
    function(object) {
        # Pattern matching against Ensembl transcript IDs
        # http://www.ensembl.org/info/genome/stable_ids/index.html
        # Examples: ENST (human); ENSMUST (mouse)
        assert_is_character(object)
        assert_all_are_not_na(object)
        assert_all_are_non_missing_nor_empty_character(object)
        gsub("^(ENS.*T\\d{11})\\.\\d+$", "\\1", object)
    }
)



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    "stripTranscriptVersions",
    signature("dgCMatrix"),
    .stripTranscriptsVersions.dim
)



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    "stripTranscriptVersions",
    signature("matrix"),
    .stripTranscriptsVersions.dim
)
