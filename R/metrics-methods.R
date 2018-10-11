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
#' @family SummarizedExperiment Functions
#' @author Michael Steinbaugh, Rory Kirchner
#' @export
#'
#' @inheritParams general
#' @param fun `string`. Mathematical function name to apply. Uses [match.arg()].
#'
#' @return `grouped_df`. Grouped by `sampleID` column.
#'
#' @examples
#' data(rse_small, sce_small)
#'
#' ## SummarizedExperiment ====
#' x <- metrics(rse_small)
#' print(x)
#'
#' ## SingleCellExperiment ====
#' x <- metrics(sce_small)
#' print(x)
#'
#' x <- metricsPerSample(sce_small, fun = "mean")
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



.metricsPerSample.SCE <-  # nolint
    function(
        object,
        fun = c("mean", "median", "sum")
    ) {
        fun <- match.arg(fun)
        message(paste("Calculating", fun, "per sample..."))
        # Consider using `getFromNamespace()` here instead.
        FUN <- get(fun, inherits = TRUE)  # nolint
        assert_is_function(FUN)
        metrics <- metrics(object)
        assert_is_all_of(metrics, "grouped_df")
        if (fun == "sum") {
            pattern <- "^n[A-Z0-9]"
            if (!any(grepl(pattern, colnames(metrics)))) {
                stop(paste(
                    "`sum` method only applies to metrics columns",
                    "prefixed with `n` (e.g. `nUMI`)."
                ), call. = FALSE)
            }
            # Sum only the `n*` columns containing counts.
            data <- select(metrics, matches(pattern))
        } else {
            # Summarize all numeric columns.
            data <- select_if(metrics, is.numeric)
        }
        assert_is_non_empty(data)
        sampleData <- sampleData(object) %>%
            as_tibble(rownames = "sampleID") %>%
            mutate(!!sym("sampleID") := as.factor(!!sym("sampleID")))
        data %>%
            summarize_all(FUN) %>%
            left_join(sampleData, by = "sampleID") %>%
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



#' @rdname metrics
#' @export
setMethod(
    f = "metricsPerSample",
    signature = signature("SingleCellExperiment"),
    definition = .metricsPerSample.SCE
)
