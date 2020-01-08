#' All generic functions
#' @include AllGenerics.R
#' @noRd
NULL



#' @rdname Ensembl2Entrez
#' @export
setGeneric(
    name = "Ensembl2Entrez",
    def = function(object, ...) {
        standardGeneric("Ensembl2Entrez")
    }
)



#' @rdname Gene2Symbol
#' @export
setGeneric(
    name = "Gene2Symbol",
    def = function(object, ...) {
        standardGeneric("Gene2Symbol")
    }
)



#' @rdname Tx2Gene
#' @export
setGeneric(
    name = "Tx2Gene",
    def = function(object, ...) {
        standardGeneric("Tx2Gene")
    }
)



#' @rdname integerCounts
#' @export
setGeneric(
    name = "integerCounts",
    def = function(object, ...) {
        standardGeneric("integerCounts")
    }
)



#' @rdname makeSampleData
#' @export
setGeneric(
    name = "makeSampleData",
    def = function(object, ...) {
        standardGeneric("makeSampleData")
    }
)



#' @rdname makeSingleCellExperiment
#' @export
setGeneric(
    name = "makeSingleCellExperiment",
    def = function(assays, ...) {
        standardGeneric("makeSingleCellExperiment")
    }
)



#' @rdname makeSummarizedExperiment
#' @export
setGeneric(
    name = "makeSummarizedExperiment",
    def = function(assays, ...) {
        standardGeneric("makeSummarizedExperiment")
    }
)
