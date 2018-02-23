#' Transcript Annotations
#'
#' @rdname transcripts
#' @name transcripts
#'
#' @importFrom ensembldb transcripts
#'
#' @inherit ensemblAnnotations
#'
#' @examples
#' data <- transcripts("Homo sapiens")
#' summary(data)
#' colnames(mcols(data))
NULL



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
        ensemblAnnotations(
            organism = x,
            format = "transcripts",
            genomeBuild = genomeBuild,
            release = release,
            return = return)
    }
)
