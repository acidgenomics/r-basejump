#' Transcript Annotations
#'
#' @rdname transcripts-method
#' @name transcripts-method
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



# Methods ======================================================================
#' @rdname transcripts-method
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
            return = return
        )
    }
)
