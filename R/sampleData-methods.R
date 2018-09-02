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
#' @return `DataFrame` containing metadata that describes the samples.
#'
#' @examples
#' # SummarizedExperiment ====
#' sampleData(rse_bcb) %>% glimpse()
#'
#' # Assignment support
#' x <- rse_bcb
#' sampleData(x)[["test"]] <- seq_len(ncol(x))
#' # `test` column should be now defined
#' glimpse(sampleData(x))
NULL



#' @rdname sampleData
#' @export
setMethod(
    "sampleData",
    signature("SummarizedExperiment"),
    function(object, ...) {
        # Legacy arguments -----------------------------------------------------
        # nocov start
        args <- matchS4Args()
        # clean
        if ("clean" %in% names(args)) {
            warning(paste(
                "`clean` argument is deprecated for `SummarizedExperiment`.",
                "Improved `bcbioRNASeq` method is provided in v0.2.6.",
                sep = "\n"
            ))
        }
        # return
        if ("return" %in% names(args)) {
            stop(paste(
                "`return` argument is defunct.\n",
                "`sampleData()` always returns `DataFrame` class.\n",
                "Use a separation coercion call for `data.frame`",
                "(e.g. `as.data.frame()`)."
            ))
        }
        # nocov end

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
    "sampleData<-",
    signature(
        object = "SummarizedExperiment",
        value = "DataFrame"
    ),
    function(object, value) {
        colData(object) <- value
        validObject(object)
        object
    }
)
