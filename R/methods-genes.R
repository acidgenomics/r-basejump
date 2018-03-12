# FIXME This is breaking pkgdown

#' @rdname ensembl
#' @usage NULL
#' @importFrom GenomicFeatures genes
#' @export
setMethod(
    "genes",
    signature("character"),
    function(
        x,
        genomeBuild = NULL,
        release = NULL,
        return = c("GRanges", "DataFrame", "data.frame")
    ) {
        ensembl(
            organism = x,
            format = "genes",
            genomeBuild = genomeBuild,
            release = release,
            return = return
        )
    }
)
