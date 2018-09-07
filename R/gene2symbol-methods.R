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
#' x <-gene2symbol(rse_small)
#' print(x)
#'
#' # New gene2symbol object ====
#' x <- new("gene2symbol", gene2symbol(rse_small))
#' class(x)
#' validObject(x)
NULL



#' @rdname gene2symbol
#' @export
setMethod(
    f = "gene2symbol",
    signature = signature("SummarizedExperiment"),
    definition = function(object) {
        validObject(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        # Early return `NULL` if object doesn't contain mappings.
        if (!all(c("geneID", "geneName") %in% colnames(data))) {
            stop("Object does not contain gene-to-symbol mappings")
        }
        .makeGene2symbol(data)
    }
)
