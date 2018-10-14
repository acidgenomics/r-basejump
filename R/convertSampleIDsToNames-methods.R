#' Convert Sample Identifiers to Names
#'
#' @name convertSampleIDsToNames
#' @export
#'
#' @inheritParams general
#'
#' @examples
#' data(rse_small)
#' convertSampleIDsToNames(rse_small)
NULL



# Designed to work with `export(human = TRUE)` mode.
.convertSampleIDsToNames.SE <-  # nolint
    function(object) {
        validObject(object)
        colData <- colData(object)
        stopifnot(!"sampleID" %in% colnames(colData))
        sampleNames <- sampleNames(object)
        assert_are_identical(names(sampleNames), colnames(object))
        if (identical(as.character(sampleNames), colnames(object))) {
            message("Returning with samples unmodified.")
        } else {
            colnames <- as.character(sampleNames)
            assert_has_no_duplicates(colnames)
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
    definition = .convertSampleIDsToNames.SE
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
