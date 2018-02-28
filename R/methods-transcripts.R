#' Transcript Annotations
#'
#' @rdname transcripts
#' @name transcripts
#' @family Gene Functions
#'
#' @inherit ensembl
#' @inheritParams general
#'
#' @examples
#' data <- transcripts("Homo sapiens")
#' summary(data)
#' colnames(mcols(data))
NULL

#' @importFrom ensembldb transcripts
#' @export
ensembldb::transcripts



# Methods ======================================================================
#' @rdname transcripts
#' @export
setMethod(
    "transcripts",
    signature("character"),
    function(
        x,
        genomeBuild = NULL,
        release = NULL,
        return = "GRanges") {
        ensembl(
            organism = x,
            format = "transcripts",
            genomeBuild = genomeBuild,
            release = release,
            return = return)
    }
)
