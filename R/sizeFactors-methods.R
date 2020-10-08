#' Size factors
#'
#' @name sizeFactors
#' @note Updated 2019-08-19.
#'
#' @importMethodsFrom SingleCellExperiment sizeFactors sizeFactors<-
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams SingleCellExperiment::sizeFactors
#' @param ... Additional arguments.
#'
#' @return `numeric`.
#'   Names correspond to object column names.
#'
#' @seealso
#' - `DESeq2::sizeFactors()`.
#' - `DESeq2::estimateSizeFactors()`.
#' - `DESeq2::estimateSizeFactorsForMatrix()`.
#' - `SingleCellExperiment::sizeFactors()`.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "AcidTest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' object <- estimateSizeFactors(object)
#' head(sizeFactors(object))
#' mean(sizeFactors(object))
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' object <- estimateSizeFactors(object)
#' head(sizeFactors(object))
#' mean(sizeFactors(object))
NULL



## If exporting a numeric value signature for SE, the SE method will mask SCE
## ANY value method. In this case, we need to export a corresponding SCE numeric
## method.
##
## See also:
## - https://github.com/drisso/SingleCellExperiment/pull/34



## nolint start
##
## SE methods are modified versions of the DESeqDataSet methods.
##
## > getMethod(
## >     f = "sizeFactors",
## >     signature = "DESeqDataSet",
## >     where = asNamespace("DESeq2")
## > )
##
## > getMethod(
## >     f = "sizeFactors<-",
## >     signature = signature(
## >         object = "DESeqDataSet",
## >         value = "numeric"
## >     ),
## >     where = asNamespace("DESeq2")
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
`sizeFactors<-,SummarizedExperiment,ANY` <-  # nolint
    function(object, value) {
        if (!is.null(value)) {
            assert(
                all(!is.na(value)),
                all(is.finite(value)),
                all(value > 0L)
            )
            value <- unname(value)
        }
        colData(object)[["sizeFactor"]] <- value
        validObject(object)
        object
    }



#' @rdname sizeFactors
#' @export
setReplaceMethod(
    f = "sizeFactors",
    signature = signature(
        object = "SummarizedExperiment",
        value = "ANY"
    ),
    definition = `sizeFactors<-,SummarizedExperiment,ANY`
)
