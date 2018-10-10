#' Strip Transcript Versions
#'
#' @note This function is strict, and will only strip Ensembl transcript IDs
#'   beginning with "ENS".
#'
#' @name stripTranscriptVersions
#' @family Sanitization Functions
#' @family Transcript-Level Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return Modified object, containing transcript identifiers without version
#'   numbers.
#'
#' @examples
#' # Ensembl (modify; contains versions)
#' stripTranscriptVersions(c(
#'     "ENSMUST00000000001.1",
#'     "ENSMUST00000000001-1",
#'     "ENSMUST00000000001_1"
#' ))
#'
#' # WormBase (keep; doesn't contain versions)
#'stripTranscriptVersions("cTel79B.1")
NULL



.stripTranscriptVersions.character <-  # nolint
    function(object) {
        # Pattern matching against Ensembl transcript IDs.
        # http://www.ensembl.org/info/genome/stable_ids/index.html
        # Examples: ENST (human); ENSMUST (mouse).
        assert_is_character(object)
        assert_all_are_not_na(object)
        assert_all_are_non_missing_nor_empty_character(object)
        # punct will match `-` or `_` here.
        gsub(
            pattern = "^(ENS.*[GT][[:digit:]]{11})[[:punct:]][[:digit:]]+$",
            replacement = "\\1",
            object
        )
    }



.stripTranscriptVersions.matrix <-  # nolint
    function(object) {
        rownames <- rownames(object)
        rownames <- stripTranscriptVersions(rownames)
        rownames(object) <- rownames
        object
    }



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("character"),
    definition = .stripTranscriptVersions.character
)



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("matrix"),
    definition = .stripTranscriptVersions.matrix
)



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("sparseMatrix"),
    definition = getMethod(
        f = "stripTranscriptVersions",
        signature = signature("matrix")
    )
)



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("SummarizedExperiment"),
    definition = getMethod(
        f = "stripTranscriptVersions",
        signature = signature("matrix")
    )
)
