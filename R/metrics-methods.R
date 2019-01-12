#' @name metrics
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit bioverbs::metrics
#' @inheritParams params
#'
#' @details
#' [metrics()] takes data stored in [`colData()`][SummarizedExperiment::colData]
#' and consistently returns a tibble grouped by sample by default (`sampleID`).
#' It always returns `sampleName` and `interestingGroups` columns, even when
#' these columns are not defined in `colData`. This is designed to integrate
#' with plotting functions that use ggplot2 internally.
#'
#' @param fun `character(1)`.
#'   Mathematical function name to apply.
#'   Uses [`match.arg()`][base::match.arg] internally.
#'
#' @return
#' - `"tibble"`: `grouped_df`.
#'     Grouped by `sampleID` column.
#' - `"DataFrame"`: `DataFrame`.
#'     Row names are identical to the column names of the object, like
#'     [`colData()`][SummarizedExperiment::colData].
#'
#' @examples
#' data(rse, sce)
#'
#' ## SummarizedExperiment ====
#' x <- metrics(rse)
#' print(x)
#'
#' ## SingleCellExperiment ====
#' x <- metrics(sce)
#' print(x)
#'
#' x <- metricsPerSample(sce, fun = "mean")
#' print(x)
NULL



#' @importFrom bioverbs metrics
#' @aliases NULL
#' @export
bioverbs::metrics

#' @importFrom bioverbs metricsPerSample
#' @aliases NULL
#' @export
bioverbs::metricsPerSample



metrics.SummarizedExperiment <-  # nolint
    function(object, return = c("tibble", "DataFrame")) {
        validObject(object)
        return <- match.arg(return)
        data <- sampleData(object) %>%
            as_tibble(rownames = "sampleID") %>%
            group_by(!!sym("sampleID"))
        if (return == "tibble") {
            data
        } else {
            data <- as(data, "DataFrame")
            rownames(data) <- data[["sampleID"]]
            data[["sampleID"]] <- NULL
            data
        }
    }



#' @describeIn metrics Metrics are sample level. `sampleID` column corresponds
#'   to `colnames`.
#' @export
setMethod(
    f = "metrics",
    signature = signature("SummarizedExperiment"),
    definition = metrics.SummarizedExperiment
)



metrics.SingleCellExperiment <-  # nolint
    function(object, return = c("tibble", "DataFrame")) {
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
        if (return == "tibble") {
            data
        } else {
            data <- as(data, "DataFrame")
            rownames(data) <- data[["cellID"]]
            data[["cellID"]] <- NULL
            data
        }
    }



#' @describeIn metrics Metrics are cell level. `cellID` column corresponds to
#'   `colnames`. Tibble is returned grouped by sample (`sampleID` column).
#' @export
setMethod(
    f = "metrics",
    signature = signature("SingleCellExperiment"),
    definition = metrics.SingleCellExperiment
)



.metricsPerSample.SingleCellExperiment <-  # nolint
    function(
        object,
        fun = c("mean", "median", "sum")
    ) {
        fun <- match.arg(fun)
        message(paste("Calculating", fun, "per sample."))
        # Consider using `getFromNamespace` here instead.
        # Note that we're using uppercase here, because `fun` is matched arg.
        FUN <- get(fun, inherits = TRUE)  # nolint
        assert(is.function(FUN))
        metrics <- metrics(object)
        assert(is(metrics, "grouped_df"))
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
        assert(hasLength(data))
        sampleData <- sampleData(object) %>%
            as_tibble(rownames = "sampleID") %>%
            mutate(!!sym("sampleID") := as.factor(!!sym("sampleID")))
        data %>%
            summarise_all(FUN) %>%
            left_join(sampleData, by = "sampleID") %>%
            group_by(!!sym("sampleID"))
    }



#' @rdname metrics
#' @export
setMethod(
    f = "metricsPerSample",
    signature = signature("SingleCellExperiment"),
    definition = .metricsPerSample.SingleCellExperiment
)
