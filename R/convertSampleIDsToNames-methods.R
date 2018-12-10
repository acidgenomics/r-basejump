#' Convert Sample Identifiers to Names
#'
#' @name convertSampleIDsToNames
#' @inheritParams params
#'
#' @return Varies, depending on the method.
#'
#' @examples
#' data(rse)
#' convertSampleIDsToNames(rse)
NULL



# Designed to work with `export(human = TRUE)` mode.
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
            assertHasNoDuplicates(colnames)
            colnames(object) <- colnames
            colData(object)[["sampleName"]] <- NULL
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



#' @rdname convertSampleIDsToNames
#' @export
setMethod(
    f = "convertSampleIDsToNames",
    signature = signature("SingleCellExperiment"),
    definition = function(object) {
        message(paste(
            "Returning with samples unmodified,",
            "since object contains cells."
        ))
        object
    }
)
