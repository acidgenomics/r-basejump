#' Metrics per Sample
#'
#' Calculate summary statistics per sample.
#'
#' @name metricsPerSample
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#' @param f `string`. Mathematical function name to apply.
#'
#' @return `tbl_df`.
#'
#' @examples
#' x <- metricsPerSample(sce_small, f = "mean")
#' print(x)
NULL



.metricsPerSample.SCE <-  # nolint
    function(
        object,
        f = c("mean", "median", "sum")
    ) {
        f <- match.arg(f)
        message(paste("Calculating", f, "per sample"))
        fxn <- get(f)
        assert_is_function(fxn)
        metrics <- metrics(object)
        assert_is_all_of(metrics, "grouped_df")
        if (f == "sum") {
            pattern <- "^n[A-Z0-9]"
            if (!any(grepl(pattern, colnames(metrics)))) {
                stop(paste(
                    "`sum` method only applies to metrics columns",
                    "prefixed with `n` (e.g. `nUMI`)"
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
            summarize_all(fxn) %>%
            arrange(!!sym("sampleID")) %>%
            left_join(sampleData, by = "sampleID")
    }



#' @rdname metricsPerSample
#' @export
setMethod(
    f = "metricsPerSample",
    signature = signature("SingleCellExperiment"),
    definition = .metricsPerSample.SCE
)
