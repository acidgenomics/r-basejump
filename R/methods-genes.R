#' Gene Annotations
#'
#' @rdname genes
#' @name genes
#' @family Gene Functions
#'
#' @importFrom ensembldb genes
#'
#' @inherit ensembl
#' @inheritParams general
#'
#' @export
#'
#' @examples
#' # GRanges return
#' data <- genes("Homo sapiens")
#' summary(data)
#' colnames(mcols(data))
#'
#' # Legacy GRCh37/hg19 genome build support
#' genes("Homo sapiens", genomeBuild = "GRCh37") %>% glimpse()
NULL
ensembldb::genes -> genes



# Methods ======================================================================
#' @rdname genes
#' @export
setMethod(
    "genes",
    signature("character"),
    function(
        x,
        genomeBuild = NULL,
        release = NULL,
        uniqueSymbol = FALSE,
        return = c("GRanges", "DataFrame", "data.frame")
    ) {
        ensembl(
            organism = x,
            format = "genes",
            genomeBuild = genomeBuild,
            release = release,
            uniqueSymbol = uniqueSymbol,
            return = return
        )
    }
)
