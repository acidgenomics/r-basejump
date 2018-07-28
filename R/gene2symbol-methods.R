#' Gene to Symbol Mappings
#'
#' @name gene2symbol
#' @family Data Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `data.frame` containing gene identifier and gene name (aka symbol)
#'   mappings.
#'
#' @examples
#' # SummarizedExperiment ====
#' gene2symbol(rse_bcb) %>% head()
NULL



# Methods ======================================================================
#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("SummarizedExperiment"),
    function(object) {
        validObject(object)
        data <- as.data.frame(rowData(object))
        cols <- c("geneID", "geneName")
        if (!all(cols %in% colnames(data))) {
            warning("Object does not contain gene-to-symbol mappings")
            return(NULL)
        }
        data <- data[, cols]
        rownames(data) <- rownames(object)
        assertIsGene2symbol(data)
        data
    }
)
