# FIXME Improve documentation for SE vs. SCE.



#' Metrics
#'
#' By default, [metrics()] returns [colData()] with an `interestingGroups`
#' column automatically added. This is useful for `ggplot2` scripting. Column
#' names are always sorted alphabetically.
#'
#' @section SingleCellExperiment:
#'
#' Returns `data.frame` with cellular barcodes as rows. If `recalculate = TRUE`,
#' returns the original object class (e.g. `SingleCellExperiment`), with updated
#' metrics in [colData()].
#'
#' @name metrics
#' @family Data Functions
#' @author Michael Steinbaugh, Rory Kirchner
#' @export
#'
#' @inheritParams general
#'
#' @return `grouped_df`. Grouped by `sampleID` column, which corresponds to the
#'   column names for `SummarizedExperiment`.
#'
#' @examples
#' # SummarizedExperiment ====
#' x <- metrics(rse_small)
#' glimpse(x)
#'
#' # SingleCellExperiment ====
#' x <- metrics(sce_small)
#' glimpse(x)
NULL



.metrics.SE <-  # nolint
    function(object) {
        validObject(object)
        sampleData(object) %>%
            as_tibble(rownames = "sampleID") %>%
            group_by(!!sym("sampleID"))
    }



.metrics.SCE <-  # nolint
    function(object) {
        validObject(object)

        colData <- colData(object)

        data <- as.data.frame(colData)
        if (!all(metricsCols %in% colnames(data))) {
            stop(paste(
                "Missing metrics:",
                setdiff(metricsCols, colnames(data)),
                "Run `metrics(object, recalculate = TRUE)`."
            ))
        }
        if (!"sampleName" %in% colnames(data)) {
            data[["sampleID"]] <- factor("unknown")
            data[["sampleName"]] <- factor("unknown")
        }
        interestingGroups <- matchInterestingGroups(
            object = object,
            interestingGroups = interestingGroups
        )
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = interestingGroups
        )
        assert_is_subset(
            x = c("sampleName", "interestingGroups"),
            y = colnames(data)
        )

        data
    }



#' @rdname metrics
#' @export
setMethod(
    f = "metrics",
    signature = signature("SummarizedExperiment"),
    definition = .metrics.SE
)



#' @rdname metrics
#' @export
setMethod(
    f = "metrics",
    signature = signature("SingleCellExperiment"),
    definition = .metrics.SCE
)
