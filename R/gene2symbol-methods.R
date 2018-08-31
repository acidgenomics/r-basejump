#' Gene to Symbol Mappings
#'
#' @note This function will make any duplicated symbols unique by applying
#' [base::make.unique()], which will add ".1" to the end of the gene name.
#'
#' @name gene2symbol
#' @family Data Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `DataFrame`.
#'
#' @examples
#' # SummarizedExperiment ====
#' gene2symbol(rse_bcb) %>% head()
NULL



#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("SummarizedExperiment"),
    function(object) {
        validObject(object)
        data <- rowData(object)
        cols <- c("geneID", "geneName")
        # Early return `NULL` if object doesn't contain mappings.
        if (!all(cols %in% colnames(data))) {
            warning("Object does not contain gene-to-symbol mappings")
            return(NULL)
        }
        .makeGene2symbol(data)
    }
)
