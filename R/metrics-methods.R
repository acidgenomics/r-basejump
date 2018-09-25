#' Metrics
#'
#' This function takes data stored in [colData()] and consistently returns a
#' tibble grouped by sample, to be used for plotting with `ggplot2`.
#'
#' @section SingleCellExperiment:
#'
#' Note that metrics are cell level but grouped by sample.
#'
#' @name metrics
#' @family Data Functions
#' @author Michael Steinbaugh, Rory Kirchner
#' @export
#'
#' @inheritParams general
#'
#' @return `grouped_df`. Grouped by `sampleID` column.
#'
#' @examples
#' # SummarizedExperiment ====
#' x <- metrics(rse_small)
#' print(x)
#'
#' # SingleCellExperiment ====
#' x <- metrics(sce_small)
#' print(x)
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
        assert_is_subset("sampleID", colnames(colData))
        if (!"sampleName" %in% colnames(colData)) {
            colData[["sampleName"]] <- colData[["sampleID"]]
        }
        colData %>%
            uniteInterestingGroups(
                interestingGroups = matchInterestingGroups(object)
            ) %>%
            as_tibble(rownames = "cellID") %>%
            group_by(!!sym("sampleID"))
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
