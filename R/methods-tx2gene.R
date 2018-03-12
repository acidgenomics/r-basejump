#' Transcript to Gene Mappings
#'
#' @name tx2gene
#' @family Gene Functions
#'
#' @inheritParams general
#' @inheritParams ensembl
#'
#' @return `data.frame`.
#'
#' @examples
#' tx2gene("Homo sapiens") %>% glimpse()
NULL



# Methods ======================================================================
#' @rdname tx2gene
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
