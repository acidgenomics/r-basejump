#' @importFrom GenomicFeatures genes
#' @export
GenomicFeatures::genes



# Methods ======================================================================
#' @rdname ensembl
#' @usage NULL
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
