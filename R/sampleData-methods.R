# Deprecate `clean` argument once bcbioRNASeq package is updated with method
# support



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
#'
#' @inheritParams general
#'
#' @param clean `boolean`. Deprecated argument, providing only factor column
#'   return.
#'
#' @return `DataFrame` containing metadata that describes the samples.
#'
#' @examples
#' # SummarizedExperiment ====
#' sampleData(rse_bcb) %>% glimpse()
#'
#' # Assignment support
#' x <- rse_dds
#' sampleData(x)[["test"]] <- seq_len(ncol(x))
#' # `test` column should be now defined
#' glimpse(sampleData(x))
NULL



# Methods ======================================================================
#' @rdname sampleData
#' @export
setMethod(
    "sampleData",
    signature("SummarizedExperiment"),
    function(object, clean = FALSE) {
        # nocov start
        if (isTRUE(clean)) {
            warning(paste(
                "`clean = TRUE` argument is deprecated for",
                "`SummarizedExperiment`. `bcbioRNASeq` method support is",
                "provided in the upcoming release."
            ))
        }
        # nocov end
        colData(object)
    }
)



#' @rdname sampleData
#' @export
setMethod(
    "sampleData<-",
    signature(
        object = "SummarizedExperiment",
        value = "DataFrame"
    ),
    function(object, value) {
        colData(object) <- as(value, "DataFrame")
        object
    }
)



#' @rdname sampleData
#' @export
setMethod(
    "sampleData<-",
    signature(
        object = "SummarizedExperiment",
        value = "data.frame"
    ),
    getMethod(
        "sampleData<-",
        signature(
            object = "SummarizedExperiment",
            value = "DataFrame"
        )
    )
)
