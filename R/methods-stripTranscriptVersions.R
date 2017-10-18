#' Strip Transcript Versions
#' 
#' @rdname stripTranscriptVersions
#' @name stripTranscriptVersions
#' @author Michael Steinbaugh
#'
#' @param sparseCounts Sparse counts matrix (`dgCMatrix`).
#' 
#' @return Transcript identifiers without version numbers.
NULL



# Constructors ====
.stripTranscriptVersions <- function(object) {
    # Pattern matching against Ensembl transcript IDs
    # http://www.ensembl.org/info/genome/stable_ids/index.html
    # Examples: ENST (human); ENSMUST (mouse)
    enstxpPattern <- "^(ENS.*T\\d{11})\\.\\d+$"
    if (any(grepl(x = object, pattern = enstxpPattern))) {
        object <- gsub(
            x = object,
            pattern = enstxpPattern,
            replacement = "\\1")
    }
    if (any(grepl(x = object, pattern = "\\.\\d+$"))) {
        stop("Transcript version removal failed", call. = FALSE)
    }
    object
}



# Methods ====
#' @rdname stripTranscriptVersions
#' @export
setMethod(
    "stripTranscriptVersions",
    signature("character"),
    .stripTranscriptVersions)
