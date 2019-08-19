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
        col <- "nCount"
        x <- metricsPerSample(sce, fun = fun)
        expect_identical(
            object = as.integer(round(x[[col]])),
            expected = expected
        )
    },
    fun = fun,
    expected = list(
        mean = c(55883L, 53993L),
        median = c(54488L, 51660L),
        sum = c(3576538L, 1943744L)
    )
)
