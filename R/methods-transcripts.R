#' Transcript Annotations
#'
#' @rdname transcripts
#' @name transcripts
#' @family Gene Functions
#'
#' @importFrom ensembldb transcripts
#'
#' @inherit ensembl
#' @inheritParams general
#'
#' @export
#'
#' @examples
#' data <- transcripts("Homo sapiens")
#' summary(data)
#' colnames(mcols(data))
ensembldb::transcripts -> transcripts



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
