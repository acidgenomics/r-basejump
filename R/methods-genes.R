#' Gene Annotations
#'
#' @rdname genes
#' @name genes
#'
#' @importFrom GenomicFeatures genes
#'
#' @section Broad Class Definitions:
#' A `broadClass` column is added, which generalizes the gene types into a
#' smaller number of semantically-meaningful groups:
#'
#'   - `coding`
#'   - `noncoding`
#'   - `pseudo`
#'   - `small`
#'   - `decaying`
#'   - `ig` (immunoglobulin)
#'   - `tcr` (T cell receptor)
#'   - `other`

#' @param uniqueSymbol Make gene symbols unique.
#'
#' @return `GRanges`, `data.frame`, or `DataFrame`.
#'
#' @examples
#' # Legacy GRCh37/hg19 genome build support
#' genes("Homo sapiens", genomeBuild = "GRCh37") %>% glimpse()
NULL



# Constructors =================================================================
.uniqueSymbol <- function(object) {
    if (is(object, "GRanges")) {
        data <- mcols(object)
    } else {
        data <- object
    }
    assert_is_subset("symbol", colnames(data))
    data[["symbol"]] <- make.unique(data[["symbol"]])
    if (is(object, "GRanges")) {
        mcols(object) <- data
    } else {
        object <- data
    }
    object
}



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
        data <- ensemblAnnotations(
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



# Aliases ======================================================================
# Changed to an alias in v0.3.2
#' @rdname genes
#' @export
annotable <- function(object, ..., return = "data.frame") {
    genes(x = object, ..., return = return)
}
