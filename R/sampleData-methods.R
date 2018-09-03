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
#' sampleData(rse_small) %>% glimpse()
#'
#' # Assignment support
#' x <- rse_small
#' sampleData(x)[["test"]] <- seq_len(ncol(x))
#' # `test` column should be now defined
#' glimpse(sampleData(x))
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
        colData(object)
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
        colData(object) <- value
        validObject(object)
        object
    }
)
