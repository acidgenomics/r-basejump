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
        x <- metricsPerSample(sce, fun = fun)
        x <- as.integer(round(x[["nCount"]]))
        expect_identical(object = x, expected = expected)
    },
    fun = fun,
    expected = list(
        mean = c(57038L, 55170L),
        median = c(56431L, 51017L),
        sum = c(2794849L, 2813670L)
    )
)
