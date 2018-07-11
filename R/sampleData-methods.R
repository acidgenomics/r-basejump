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
#' @param clean `logical`. Only return `factor` columns not defined in
#'   [metadataBlacklist].
#'
#' @return Data describing the samples.
#'
#' @seealso [metadataBlacklist].
#'
#' @examples
#' # SummarizedExperiment ====
#' sampleData(rse_bcb, clean = TRUE) %>% glimpse()
#' sampleData(rse_bcb, clean = FALSE) %>% glimpse()
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
    function(
        object,
        clean = TRUE,
        interestingGroups,
        return = c("DataFrame", "data.frame", "kable")
    ) {
        data <- colData(object)
        assert_is_a_bool(clean)
        return <- match.arg(return)

        # Only return factor columns, if desired
        if (isTRUE(clean)) {
            data <- data[, vapply(data, is.factor, logical(1L)), drop = FALSE]
            # Drop remaining blacklisted columns
            setdiff <- setdiff(colnames(data), metadataBlacklist)
            data <- data[, setdiff, drop = FALSE]
        } else {
            # Include `interestingGroups` column, if not NULL
            if (missing(interestingGroups)) {
                interestingGroups <- bcbioBase::interestingGroups(object)
            }
            if (length(interestingGroups)) {
                data <- uniteInterestingGroups(data, interestingGroups)
            }
        }

        # Return
        if (return == "kable") {
            kable(as.data.frame(data), row.names = FALSE)
        } else {
            as(data, return)
        }
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
