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
    enstxpPattern <- "^(ENS.*T\\d{11})\\.\\d+$"
    if (any(grepl(enstxpPattern, object))) {
        object <- gsub(enstxpPattern, "\\1", object)
    }
    if (any(grepl("\\.\\d+$", object))) {
        abort("Failed to sanitize Ensembl transcript identifier")
    }
    object
}



# Methods ======================================================================
#' @rdname stripTranscriptVersions
#' @export
setMethod(
    "stripTranscriptVersions",
    signature("character"),
    .stripTranscriptVersions)
