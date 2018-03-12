#' @rdname ensembl
#' @export
setMethod(
    "gene2symbol",
    signature("character"),
    function(
        object,
        genomeBuild = NULL,
        release = NULL
    ) {
        ensembl(
            organism = object,
            format = "gene2symbol",
            genomeBuild = genomeBuild,
            release = release,
            return = "data.frame"
        )
    }
)
