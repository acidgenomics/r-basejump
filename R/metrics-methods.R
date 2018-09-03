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
#' @inheritParams general
#'
#' @return `DataFrame`.
#'
#' @examples
#' x <- metrics(rse_small)
#' glimpse(x)
NULL



#' @rdname metrics
#' @export
setMethod(
    f = "metrics",
    signature = signature("SummarizedExperiment"),
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
