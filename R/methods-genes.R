#' Gene Annotations
#'
#' @rdname genes
#' @name genes
#'
#' @importFrom ensembldb genes
#'
#' @inherit ensembl
#' @inheritParams general
#'
#' @param uniqueSymbol Make gene symbols unique.
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
