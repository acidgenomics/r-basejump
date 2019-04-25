#' @name stripTranscriptVersions
#' @inherit bioverbs::stripTranscriptVersions
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @note This method is strict, and will only strip Ensembl transcript IDs
#'   beginning with "ENS".
#'
#' @examples
#' ## Ensembl (modify; contains versions)
#' stripTranscriptVersions(c(
#'     "ENSMUST00000000001.1",
#'     "ENSMUST00000000001-1",
#'     "ENSMUST00000000001_1"
#' ))
#'
#' ## WormBase (keep; doesn't contain versions)
#'stripTranscriptVersions("cTel79B.1")
NULL



# Pattern matching against Ensembl transcript IDs.
# http://www.ensembl.org/info/genome/stable_ids/index.html
# Examples: ENST (human); ENSMUST (mouse).
# `:punct:` will match `-` or `_` here.
stripTranscriptVersions.character <-  # nolint
    function(object) {
        assert(isCharacter(object))
        gsub(
            pattern = "^(ENS.*[GT][[:digit:]]{11})[[:punct:]][[:digit:]]+$",
            replacement = "\\1",
            x = object
        )
    }



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("character"),
    definition = stripTranscriptVersions.character
)



stripTranscriptVersions.matrix <-  # nolint
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
    signature = signature("matrix"),
    definition = stripTranscriptVersions.matrix
)



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("sparseMatrix"),
    definition = stripTranscriptVersions.matrix
)



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("SummarizedExperiment"),
    definition = stripTranscriptVersions.matrix
)
