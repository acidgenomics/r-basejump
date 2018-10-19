#' Metrics
#'
#' This function takes data stored in [SummarizedExperiment::colData()] and
#' consistently returns a tibble grouped by sample by default (`sampleID`).
#'
#' [metrics()] always returns `sampleName` and `interestingGroups` columns, even
#' when these columns are not defined in `colData`. This is designed to
#' integrate with plotting functions that use ggplot2 internally.
#'
#' @name metrics
#' @family SummarizedExperiment Functions
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @inheritParams general
#' @param fun `string`. Mathematical function name to apply.
#'   Uses [base::match.arg()].
#'
#' @return
#' - `"tbl_df"`: `grouped_df`. Grouped by `sampleID` column.
#' - `"DataFrame"`: `DataFrame`. Rownames are identical to [base::colnames()],
#'   of the object, like [SummarizedExperiment::colData()].
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



.metrics.SummarizedExperiment <-  # nolint
    function(object, return = c("tbl_df", "DataFrame")) {
        validObject(object)
        return <- match.arg(return)
        data <- sampleData(object) %>%
            as_tibble(rownames = "sampleID") %>%
            group_by(!!sym("sampleID"))
        if (return == "tbl_df") {
            data
        } else {
            data <- as(data, "DataFrame")
            rownames(data) <- data[["sampleID"]]
            data[["sampleID"]] <- NULL
            data
        }
    }



.metrics.SingleCellExperiment <-  # nolint
    function(object, return = c("tbl_df", "DataFrame")) {
        validObject(object)
        return <- match.arg(return)
        data <- colData(object)
        if (!"sampleID" %in% colnames(data)) {
            data[["sampleID"]] <- factor("unknown")
        }
        if (!"sampleName" %in% colnames(data)) {
            data[["sampleName"]] <- data[["sampleID"]]
        }
        data <- data %>%
            uniteInterestingGroups(
                interestingGroups = matchInterestingGroups(object)
            ) %>%
            as_tibble(rownames = "cellID") %>%
            group_by(!!sym("sampleID"))
        if (return == "tbl_df") {
            data
        } else {
            data <- as(data, "DataFrame")
            rownames(data) <- data[["cellID"]]
            data[["cellID"]] <- NULL
            data
        }
    }



.metricsPerSample.SingleCellExperiment <-  # nolint
    function(
        object,
        fun = c("mean", "median", "sum")
    ) {
        fun <- match.arg(fun)
        message(paste("Calculating", fun, "per sample."))
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



#' @describeIn metrics Metrics are sample level. `sampleID` column corresponds
#'   to [base::colnames()].
#' @export
setMethod(
    f = "metrics",
    signature = signature("SummarizedExperiment"),
    definition = .metrics.SummarizedExperiment
)



#' @describeIn metrics Metrics are cell level. `cellID` column corresponds to
#'   [base::colnames()]. Tibble is returned grouped by sample
#'   (`sampleID` column).
#' @export
setMethod(
    f = "metrics",
    signature = signature("SingleCellExperiment"),
    definition = .metrics.SingleCellExperiment
)



#' @rdname metrics
#' @export
setMethod(
    f = "metricsPerSample",
    signature = signature("SingleCellExperiment"),
    definition = .metricsPerSample.SingleCellExperiment
)
