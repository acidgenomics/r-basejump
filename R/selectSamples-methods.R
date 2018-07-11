#' Select Samples
#'
#' Sample selection utility function designed for interactive use.
#'
#' @name selectSamples
#' @family Data Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param ... Selection arguments that map to the column names of
#'   [sampleData()]. `atomic` values are supported. Avoid using `logical` or
#'   `numeric` indices (e.g. [base::which()] calls) here, for improved code
#'   readability.
#'
#' @return `SummarizedExperiment`.
#'
#' @examples
#' # SummarizedExperiment ====
#' x <- selectSamples(rse_dds, condition = "A")
#' show(x)
#' colnames(x)
NULL



# Methods ======================================================================
#' @rdname selectSamples
#' @export
setMethod(
    "selectSamples",
    signature("SummarizedExperiment"),
    function(object, ...) {
        validObject(object)
        args <- list(...)
        invisible(lapply(args, assert_is_atomic))

        # Match the arguments against the sample metadata
        colData <- colData(object)
        assert_is_subset(names(args), colnames(colData))

        # Obtain the sample identifiers
        list <- mapply(
            col = names(args),
            arg = args,
            MoreArgs = list(data = colData),
            FUN = function(col, arg, data) {
                rownames(data[data[[col]] %in% arg, , drop = FALSE])
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
        samples <- Reduce(f = intersect, x = list) %>%
            as.character() %>%
            sort()
        assert_is_non_empty(samples)

        object[, samples]
    }
)
