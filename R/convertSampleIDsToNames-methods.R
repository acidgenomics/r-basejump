#' @name convertSampleIDsToNames
#' @inherit AcidGenerics::convertSampleIDsToNames
#' @note Updated 2020-01-20.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return Varies, depending on the method.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#' convertSampleIDsToNames(rse)
NULL



## NULL assignment into a column name doesn't work for DataFrame class.
## You can see this cryptic error on some R installations:
##
## nolint start
## > colData(object)[["sampleName"]] <- NULL
## Error in replaceROWS(x, if (missing(i)) nsbs else i, value) :
##   appending gaps is not supported
## nolint end
##
## Updated 2020-01-20.
`convertSampleIDsToNames,SummarizedExperiment` <-  # nolint
    function(object) {
        validObject(object)
        sampleNames <- sampleNames(object)
        if (
            identical(as.character(sampleNames), colnames(object)) ||
            !identical(names(sampleNames), colnames(object))
        ) {
            cli_alert_warning("Returning with the sample names unmodified.")
        } else {
            colnames <- as.character(sampleNames)
            assert(hasNoDuplicates(colnames))
            colnames(object) <- colnames
        }
        ## Note that we need to allow invalid dimnames to pass through here,
        ## so don't run validity checks.
        object
    }



#' @rdname convertSampleIDsToNames
#' @export
setMethod(
    f = "convertSampleIDsToNames",
    signature = signature("SummarizedExperiment"),
    definition = `convertSampleIDsToNames,SummarizedExperiment`
)



## Updated 2019-07-22.
`convertSampleIDsToNames,SingleCellExperiment` <-  # nolint
    function(object) {
        cli_alert_warning(paste(
            "{.var SingleCellExperiment} contains cells instead of samples.",
            "Returning with column names unmodified."
        ))
        object
    }



#' @rdname convertSampleIDsToNames
#' @export
setMethod(
    f = "convertSampleIDsToNames",
    signature = signature("SingleCellExperiment"),
    definition = `convertSampleIDsToNames,SingleCellExperiment`
)
