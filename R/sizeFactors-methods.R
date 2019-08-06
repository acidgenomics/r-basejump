#' Size factors
#'
#' @name sizeFactors
#' @note Updated 2019-08-06.
#'
#' @importMethodsFrom SingleCellExperiment sizeFactors sizeFactors<-
#'
#' @inheritParams acidroxygen::params
#' @param value Value to be assigned to corresponding components of object.
#' @param ... Additional arguments.
#'
#' @return `numeric`.
#'   Names correspond to object column names.
#'
#' @seealso
#' - `DESeq2::sizeFactors()`.
#' - `DESeq2::estimateSizeFactors()`.
#' - `DESeq2::estimateSizeFactorsForMatrix()`.
#' - `SummarizedExperiment::colData()`.
#'
#' @examples
#' data(SingleCellExperiment, package = "acidtest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' object <- estimateSizeFactors(object)
#' head(sizeFactors(object))
NULL



#' @rdname sizeFactors
#' @name sizeFactors
#' @importFrom BiocGenerics sizeFactors
#' @usage sizeFactors(object, ...)
#' @export
NULL

#' @rdname sizeFactors
#' @name sizeFactors<-
#' @importFrom BiocGenerics sizeFactors<-
#' @usage sizeFactors(object, ...) <- value
#' @export
NULL



## nolint start
##
## SE methods are modified versions of the DESeqDataSet methods.
##
## > getMethod(
## >     f = "sizeFactors",
## >     signature = "DESeqDataSet"
## > )
##
## > getMethod(
## >     f = "sizeFactors<-",
## >     signature = signature(
## >         object = "DESeqDataSet",
## >         value = "numeric"
## >     )
## > )
##
## nolint end



## Updated 2019-08-06.
`sizeFactors,SummarizedExperiment` <-  # nolint
    function(object) {
        if (!"sizeFactor" %in% names(colData(object))) {
            return(NULL)
        }
        sf <- colData(object)[["sizeFactor"]]
        names(sf) <- colnames(object)
        sf
    }



#' @rdname sizeFactors
#' @export
setMethod(
    f = "sizeFactors",
    signature = signature("SummarizedExperiment"),
    definition = `sizeFactors,SummarizedExperiment`
)



## Updated 2019-08-06.
`sizeFactors<-,SummarizedExperiment,numeric` <-  # nolint
    function(object, value) {
        assert(
            all(!is.na(value)),
            all(is.finite(value)),
            all(value > 0)
        )
        colData(object)[["sizeFactor"]] <- unname(value)
        validObject(object)
        object
    }



#' @rdname sizeFactors
#' @export
setReplaceMethod(
    f = "sizeFactors",
    signature = signature(
        object = "SummarizedExperiment",
        value = "numeric"
    ),
    definition = `sizeFactors<-,SummarizedExperiment,numeric`
)
