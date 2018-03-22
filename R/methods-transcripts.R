#' @importFrom GenomicFeatures transcripts
#' @export
GenomicFeatures::transcripts



# Methods ======================================================================
#' @rdname ensembl
#' @usage NULL
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
