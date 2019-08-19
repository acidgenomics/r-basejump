#' @name metrics
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit bioverbs::metrics
#' @note These functions will error intentionally if no numeric columns are
#'   defined in `colData()`.
#' @note Updated 2019-08-18.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @details
#' [metrics()] takes data stored in [`colData()`][SummarizedExperiment::colData]
#' and consistently returns a `tbl_df` or `DataFrame` with `sampleName` and
#' `interestingGroups` columns, even when these columns are not defined in
#' `colData()`. This is designed to integrate with plotting functions that use
#' ggplot2 internally.
#'
#' @param fun `character(1)`.
#'   Mathematical function name to apply.
#'   Uses [`match.arg()`][base::match.arg] internally.
#'
#' @return Object of class determined by `return` argument.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' object <- calculateMetrics(object)
#' x <- metrics(object)
#' print(x)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' object <- calculateMetrics(object)
#' x <- metrics(object)
#' print(x)
#' x <- metricsPerSample(object, fun = "mean")
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



## Updated 2019-08-18.
`metrics,SummarizedExperiment` <-  # nolint
    function(object, return = c("tbl_df", "DataFrame")) {
        validObject(object)
        return <- match.arg(return)
        data <- sampleData(object, clean = FALSE)
        ## Decode columns that contain Rle, if necessary.
        data <- decode(data)
        switch(
            EXPR = return,
            "DataFrame" = data,
            "tbl_df" = as_tibble(data, rownames = "sampleID")
        )
    }



#' @describeIn metrics Sample-level metrics.
#' @export
setMethod(
    f = "metrics",
    signature = signature("SummarizedExperiment"),
    definition = `metrics,SummarizedExperiment`
)



## Updated 2019-08-18.
`metrics,SingleCellExperiment` <-  # nolint
    function(object, return = c("tbl_df", "DataFrame")) {
        validObject(object)
        return <- match.arg(return)
        blacklist <- c("cell", "sample")
        data <- colData(object)
        data <- data[, setdiff(colnames(data), blacklist), drop = FALSE]
        ## Decode columns that contain Rle, if necessary.
        data <- decode(data)
        ## Automatically assign `sampleID` column, if necessary.
        if (!isSubset("sampleID", colnames(data))) {
            data[["sampleID"]] <- factor("unknown")
        }
        ## Automatically assign `sampleName` column, if necessary.
        if (!isSubset("sampleName", colnames(data))) {
            data[["sampleName"]] <- data[["sampleID"]]
        }
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = matchInterestingGroups(object)
        )
        switch(
            EXPR = return,
            "DataFrame" = data,
            "tbl_df" = as_tibble(data, rownames = "cellID")
        )
    }



#' @describeIn metrics Cell-level metrics.
#' @export
setMethod(
    f = "metrics",
    signature = signature("SingleCellExperiment"),
    definition = `metrics,SingleCellExperiment`
)



## Updated 2019-08-18.
`metricsPerSample,SingleCellExperiment` <-  # nolint
    function(
        object,
        fun = c("mean", "median", "sum"),
        return = c("tbl_df", "DataFrame")
    ) {
        fun <- match.arg(fun)
        return <- match.arg(return)
        message(sprintf("Calculating %s per sample.", fun))
        ## Consider using `getFromNamespace` here instead.
        ## Note that we're using uppercase here, because `fun` is matched arg.
        FUN <- get(fun, inherits = TRUE)  # nolint
        assert(is.function(FUN))
        data <- colData(object)
        ## Decode columns that contain Rle, if necessary.
        data <- decode(data)
        # Subset the relevant metrics columns.
        if (identical(fun, "sum")) {
            pattern <- "^n[A-Z0-9]"
            if (!any(grepl(pattern, colnames(data)))) {
                stop(
                    "'sum()' method only applies to 'colData()' columns ",
                    "prefixed with 'n' (e.g. 'nCount')."
                )
            }
            ## Sum only the `n*` columns containing counts.
            ## Supress: Adding missing grouping variables: `sampleID`.
            keep <- grepl(pattern = pattern, x = colnames(data))
        } else {
            ## Summarize all numeric columns.
            keep <- bapply(data, is.numeric)
        }
        split <- split(data, f = data[["sampleID"]])
        split <- split[, keep]
        split <- DataFrameList(lapply(
            X = split,
            FUN = function(x) {
                DataFrame(lapply(X = x, FUN = FUN))
            }
        ))
        data <- unlist(split, recursive = FALSE, use.names = TRUE)
        sampleData <- sampleData(object)
        data <- data[rownames(sampleData), , drop = FALSE]
        data <- cbind(sampleData, data)
        switch(
            EXPR = return,
            "DataFrame" = data,
            "tbl_df" = as_tibble(data, rownames = "sampleID")
        )
    }



#' @describeIn metrics Sample-level metrics.
#' @export
setMethod(
    f = "metricsPerSample",
    signature = signature("SingleCellExperiment"),
    definition = `metricsPerSample,SingleCellExperiment`
)
