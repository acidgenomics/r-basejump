#' @rdname ensembl
#' @export
setMethod(
    "tx2gene",
    signature("character"),
    function(
        object,
        genomeBuild = NULL,
        release = NULL) {
        ensembl(
            object,
            format = "tx2gene",
            genomeBuild = genomeBuild,
            release = release,
            return = "data.frame"
        )
    }
)
