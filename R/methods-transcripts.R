# FIXME This is breaking pkgdown

#' @rdname ensembl
#' @usage NULL
#' @importFrom GenomicFeatures transcripts
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
