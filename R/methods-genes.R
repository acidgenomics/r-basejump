#' Gene Annotations
#'
#' @rdname genes
#' @name genes
#' @family Gene Annotation Utilities
#'
#' @importFrom ensembldb genes
#'
#' @inherit ensembl
#' @inheritParams general
#'

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
ensembldb::genes



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
        return = "GRanges") {
        assert_is_a_bool(uniqueSymbol)
        data <- ensembl(
            organism = x,
            format = "genes",
            genomeBuild = genomeBuild,
            release = release,
            return = return)
        if (isTRUE(uniqueSymbol)) {
            data <- .uniqueSymbol(data)
        }
        data
    }
)
