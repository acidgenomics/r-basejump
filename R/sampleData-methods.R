#' Sample Data
#'
#' Return the sample metadata. Columns are always sanitized to factor.
#'
#' @note This is a complement to the standard [colData()] function, but improves
#'   support for accessing sample metadata for datasets where multiple items in
#'   the columns map to a single sample (e.g. cells for a single-cell RNA-seq
#'   experiment).
#'
#' @name sampleData
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `DataFrame`. Metadata that describes the samples.
#'
#' @examples
#' # SummarizedExperiment ====
#' sampleData(rse_small)
#'
#' # Assignment support
#' x <- rse_small
#' sampleData(x)[["test"]] <- seq_len(ncol(x))
#' # `test` column should be now defined
#' sampleData(x)
NULL



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData",
    signature = signature("SummarizedExperiment"),
    definition = function(object) {
        validObject(object)
        # Require `sampleName` column to be defined.
        assert_is_subset(
            x = "sampleName",
            y = colnames(colData(object))
        )
        data <- colData(object)
        interestingGroups <- interestingGroups(object)
        if (length(interestingGroups) > 0L) {
            data <- uniteInterestingGroups(data, interestingGroups)
        }
        data
    }
)



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "DataFrame"
    ),
    definition = function(object, value) {
        # Don't allow the user to set `interestingGroups` column manually.
        value[["interestingGroups"]] <- NULL
        # Check for blacklisted columns.
        assert_are_disjoint_sets(
            colnames(value),
            c(
                "rowname",
                "sampleID"
            )
        )
        # Now safe to assign.
        colData(object) <- value
        validObject(object)
        object
    }
)
