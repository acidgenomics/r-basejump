context("calculateMetrics")

with_parameters_test_that(
    "matrix-like", {
        object <- calculateMetrics(object, prefilter = TRUE)
        expect_s4_class(object, "DataFrame")
        ## Check for run-length encoding.
        expect_true(
            all(bapply(X = object, FUN = function(x) { is(x, "Rle") }))
        )
        ## Check that expected values match.
        ## Parameterized unit test is changing `nCount` to NA.
        x <- object[1L, c("nCount", "nFeature", "nMito"), drop = TRUE]
        x <- lapply(x, decode)
        y <- list(
            nCount = 41054L,
            nFeature = 235L,
            nMito = NA_integer_
        )
        expect_identical(x, y)
    },
    object = list(
        Matrix = counts(sce),
        DelayedArray = DelayedArray(counts(sce))
    )
)

with_parameters_test_that(
    "SummarizedExperiment", {
        x <- calculateMetrics(object)
        expect_s4_class(x, "SummarizedExperiment")
    },
    object = list(
        SummarizedExperiment = rse,
        SingleCellExperiment = sce
    )
)
