#' @name convertSampleIDsToNames
#' @inherit bioverbs::convertSampleIDsToNames
#' @inheritParams params
#'
#' @return Varies, depending on the method.
#'
#' @examples
#' data(rse, package = "acidtest")
#' convertSampleIDsToNames(rse)
NULL



#' @importFrom bioverbs convertSampleIDsToNames
#' @aliases NULL
#' @export
bioverbs::convertSampleIDsToNames



# NULL assignment into a column name doesn't work for DataFrame class.
# You can see this cryptic error on some R installations:
# nolint start
# > colData(object)[["sampleName"]] <- NULL
# Error in replaceROWS(x, if (missing(i)) nsbs else i, value) :
#   appending gaps is not supported
# nolint end
convertSampleIDsToNames.SummarizedExperiment <-  # nolint
    function(object) {
        validObject(object)
        sampleNames <- sampleNames(object)
        if (
            identical(as.character(sampleNames), colnames(object)) ||
            !identical(names(sampleNames), colnames(object))
        ) {
            message("Returning with the sample names unmodified.")
        } else {
            colnames <- as.character(sampleNames)
            assert(hasNoDuplicates(colnames))
            colnames(object) <- colnames
        }
        # Note that we need to allow invalid dimnames to pass through here,
        # so don't run validity checks.
        object
    }



#' @rdname convertSampleIDsToNames
#' @export
setMethod(
    f = "convertSampleIDsToNames",
    signature = signature("SummarizedExperiment"),
    definition = convertSampleIDsToNames.SummarizedExperiment
)



convertSampleIDsToNames.SingleCellExperiment <-  # nolint
    function(object) {
        message(paste(
            "SingleCellExperiment contains cells instead of samples.",
            "Returning with column names unmodified.",
            sep = "\n"
        ))
    }



#' @rdname convertSampleIDsToNames
#' @export
setMethod(
    f = "convertSampleIDsToNames",
    signature = signature("SingleCellExperiment"),
    definition = convertSampleIDsToNames.SingleCellExperiment
)
