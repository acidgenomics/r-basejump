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



# FIXME Generate `sampleName` column if it doesn't exist.
.sampleData.SE <-  # nolint
    function(object) {
        validObject(object)
        # Require `sampleName` column to be defined.
        # FIXME
        # assert_is_subset(
        #     x = "sampleName",
        #     y = colnames(colData(object))
        # )
        data <- colData(object)
        # Generate `interestingGroups` column, if necessary.
        if (!"interestingGroups" %in% colnames(data)) {
            data <- uniteInterestingGroups(
                object = data,
                interestingGroups = matchInterestingGroups(object)
            )
        }
        data
    }



`.sampleData<-.SE` <-  # nolint
    function(object, value) {
        # Check for blacklisted columns.
        assert_are_disjoint_sets(
            colnames(value),
            c("rowname", "sampleID")
        )

        # Reslot the interesting groups column automatically.
        value[["interestingGroups"]] <- NULL
        value <- uniteInterestingGroups(
            object = value,
            interestingGroups = matchInterestingGroups(object)
        )

        # Now safe to assign and return.
        colData(object) <- value
        validObject(object)
        object
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData",
    signature = signature("SummarizedExperiment"),
    definition = .sampleData.SE
)



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "DataFrame"
    ),
    definition = `.sampleData<-.SE`
)
