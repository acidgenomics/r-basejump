#' @name metrics
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit bioverbs::metrics
#' @note Updated 2019-08-06.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
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
#' - `"tbl_df"`: `grouped_df`.
#'     Tibble grouped by `sampleID` column.
#' - `"DataFrame"`: `DataFrame`.
#'     Row names are identical to the column names of the object, like
#'     [`colData()`][SummarizedExperiment::colData].
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#' rse <- RangedSummarizedExperiment
#' sce <- SingleCellExperiment
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



#' @rdname metrics
#' @name metrics
#' @importFrom bioverbs metrics
#' @usage metrics(object, ...)
#' @export
NULL

#' @rdname metrics
#' @name metricsPerSample
#' @importFrom bioverbs metricsPerSample
#' @usage metricsPerSample(object, ...)
#' @export
NULL



## Updated 2019-07-22.
`metrics,SummarizedExperiment` <-  # nolint
    function(object, return = c("tbl_df", "DataFrame")) {
        validObject(object)
        return <- match.arg(return)
        data <- sampleData(object, clean = FALSE)
        if (return == "tbl_df") {
            data %<>%
                as_tibble(rownames = "sampleID") %>%
                group_by(!!sym("sampleID"))
        }
        data
    }



#' @describeIn metrics Metrics are sample level. `sampleID` column corresponds
#'   to `colnames`.
#' @export
setMethod(
    f = "metrics",
    signature = signature("SummarizedExperiment"),
    definition = `metrics,SummarizedExperiment`
)



## Updated 2019-08-05.
`metrics,SingleCellExperiment` <-  # nolint
    function(object, return = c("tbl_df", "DataFrame")) {
        validObject(object)
        return <- match.arg(return)
        data <- colData(object)
        ## Strip "cell" column, which gets defined by monocle3.
        ## Consider adding a step to strip "sample" if slotted also.
        if (isSubset("cell", colnames(data))) {
            data[["cell"]] <- NULL
        }
        ## Automatically assign `sampleID` column, if necessary.
        if (!isSubset("sampleID", colnames(data))) {
            data[["sampleID"]] <- factor("unknown")
        }
        ## Automatically assign `sampleName` column, if necessary.
        if (!isSubset("sampleName", colnames(data))) {
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



#' @describeIn metrics Metrics are cell level. `cellID` column corresponds to
#'   `colnames`. Tibble is returned grouped by sample (`sampleID` column).
#' @export
setMethod(
    f = "metrics",
    signature = signature("SingleCellExperiment"),
    definition = `metrics,SingleCellExperiment`
)



## Updated 2019-07-22.
`metricsPerSample,SingleCellExperiment` <-  # nolint
    function(
        object,
        fun = c("mean", "median", "sum")
    ) {
        fun <- match.arg(fun)
        message(sprintf("Calculating %s per sample.", fun))
        ## Consider using `getFromNamespace` here instead.
        ## Note that we're using uppercase here, because `fun` is matched arg.
        FUN <- get(fun, inherits = TRUE)  # nolint
        assert(is.function(FUN))
        metrics <- metrics(object, return = "tbl_df")
        assert(is(metrics, "grouped_df"))
        if (fun == "sum") {
            pattern <- "^n[A-Z0-9]"
            if (!any(grepl(pattern, colnames(metrics)))) {
                stop(
                    "'sum()' method only applies to metrics columns ",
                    "prefixed with 'n' (e.g. 'nCount')."
                )
            }
            ## Sum only the `n*` columns containing counts.
            ## Supress: Adding missing grouping variables: `sampleID`.
            suppressMessages(
                data <- select(metrics, matches(pattern))
            )
        } else {
            ## Summarize all numeric columns.
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
    definition = `metricsPerSample,SingleCellExperiment`
)
