context("metrics")

rse <- calculateMetrics(rse)
sce <- calculateMetrics(sce)

test_that("SummarizedExperiment : tibble", {
    x <- metrics(rse, return = "tbl_df")
    expect_s3_class(x, "tbl_df")
    expect_identical(
        object = colnames(x),
        expected = c(
            "sampleID",
            "condition",
            "nCount",
            "nFeature",
            "nCoding",
            "nMito",
            "log10FeaturesPerCount",
            "mitoRatio",
            "sampleName",
            "interestingGroups"
        )
    )
})

test_that("SummarizedExperiment : DataFrame", {
    x <- metrics(rse, return = "DataFrame")
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})

test_that("SingleCellExperiment", {
    x <- metrics(sce, return = "tbl_df")
    expect_s3_class(x, "tbl_df")

    x <- metrics(sce, return = "DataFrame")
    expect_s4_class(x, "DataFrame")
})



context("metricsPerSample")

fun <- eval(formals(`metricsPerSample,SingleCellExperiment`)[["fun"]])
with_parameters_test_that(
    "SingleCellExperiment", {
        x <- sce
        x <- calculateMetrics(x)
        x <- metricsPerSample(x, fun = fun)
        x <- as.integer(round(x[["nCount"]]))
        expect_identical(object = x, expected = expected)
    },
    fun = fun,
    expected = list(
        mean = c(60587L, 54304L),
        median = c(56661L, 52534L),
        sum = c(3211137L, 2552299L)
    )
)
