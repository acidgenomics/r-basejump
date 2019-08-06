## FIXME SCE method isn't assigning into `object@int_colData` as expected.



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
`sizeFactors<-,SummarizedExperiment,numeric` <-  # nolint
    function(object, value) {
        assert(
            all(!is.na(value)),
            all(is.finite(value)),
            all(value > 0L)
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



## Need to export numeric value signature, otherwise SE method mask.
## Updated 2019-08-06.
`sizeFactors<-,SingleCellExperiment,ANY` <-  # nolint
    methodFunction(
        f = "sizeFactors<-",
        signature = signature(
            object = "SingleCellExperiment",
            value = "ANY"
        ),
        package = "SingleCellExperiment"
    )



`sizeFactors<-,SingleCellExperiment,numeric` <-  # nolint
    `sizeFactors<-,SingleCellExperiment,ANY`



#' @rdname sizeFactors
#' @export
setReplaceMethod(
    f = "sizeFactors",
    signature = signature(
        object = "SingleCellExperiment",
        value = "numeric"
    ),
    definition = `sizeFactors<-,SingleCellExperiment,numeric`
)
