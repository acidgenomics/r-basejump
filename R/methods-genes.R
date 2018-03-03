#' Gene Annotations
#'
#' @rdname genes
#' @name genes-method
#' @family Gene Functions
#'
#' @inherit ensembl
#' @inheritParams general
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
