#' Metrics
#'
#' By default, [metrics()] returns [colData()] with an `interestingGroups`
#' column automatically added. This is useful for `ggplot2` scripting. Column
#' names are always sorted alphabetically.
#'
#' @note [metrics()] Requires that the `sampleName` column is defined, and will
#' error otherwise.
#'
#' @name metrics
#' @family Data Functions
#' @author Michael Steinbaugh
#'
#' @return `DataFrame`.
#'
#' @examples
#' x <- metrics(rse_bcb)
#' glimpse(x)
NULL



#' @rdname metrics
#' @export
setMethod(
    "metrics",
    signature("SummarizedExperiment"),
    function(object) {
        validObject(object)
        interestingGroups <- interestingGroups(object)
        data <- colData(object)
        assert_is_non_empty(data)
        # Add the `interestingGroups` column.
        data <- uniteInterestingGroups(data, interestingGroups)
        assert_is_subset(
            x = c("sampleName", "interestingGroups"),
            y = colnames(data)
        )
        # Require that the columns are sorted alphabetically.
        data <- data[, sort(colnames(data))]
        data
    }
)
