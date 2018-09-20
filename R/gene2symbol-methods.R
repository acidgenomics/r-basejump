#' Gene-to-Symbol Mappings
#'
#' @note This function will make any duplicated symbols unique by applying
#' [base::make.unique()], which will add ".1" to the end of the gene name.
#'
#' @name gene2symbol
#' @family Annotation Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `gene2symbol`.
#'
#' @examples
#' # SummarizedExperiment ====
#' x <- gene2symbol(rse_small)
#' print(x)
NULL



.gene2symbol.GRanges <-  # nolint
    function(object) {
        validObject(object)
        .makeGene2symbol(object)
    }



.gene2symbol.SE <-  # nolint
    function(object) {
        validObject(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        .makeGene2symbol(data)
    }



#' @rdname gene2symbol
#' @export
setMethod(
    f = "gene2symbol",
    signature = signature("GRanges"),
    definition = .gene2symbol.GRanges
)



#' @rdname gene2symbol
#' @export
setMethod(
    f = "gene2symbol",
    signature = signature("SummarizedExperiment"),
    definition = .gene2symbol.SE
)
