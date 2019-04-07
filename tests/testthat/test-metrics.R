context("metrics")

test_that("SE : tibble", {
    x <- metrics(rse)
    expect_s3_class(x, "tbl_df")
    expect_identical(group_vars(x), "sampleID")
    expect_identical(
        object = colnames(x),
        expected = c(
            "sampleID",
            "condition",
            "sampleName",
            "interestingGroups"
        )
    )
})

test_that("SE : DataFrame", {
    x <- metrics(rse, return = "DataFrame")
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})

test_that("SCE : tibble", {
    x <- metrics(sce)
    expect_s3_class(x, "tbl_df")

})



context("metricsPerSample")

# nolint start
# fun <- eval(formals(metricsPerSample.SingleCellExperiment)[["fun"]])
# nolint end

test_that("SingleCellExperiment", {
    x <- metricsPerSample(sce, fun = "mean")
    expect_identical(
        object = round(x[["expLibSize"]], digits = 2L),
        expected = c(61185.16, 56830.14)
    )

    x <- metricsPerSample(sce, fun = "median")
    expect_identical(
        object = round(x[["expLibSize"]], digits = 2L),
        expected = c(58211.62, 55454.75)
    )

    expect_error(
        object = metricsPerSample(sce, fun = "sum"),
        expected = paste(
            "`sum` method only applies to metrics columns",
            "prefixed with `n` (e.g. `nUMI`)."
        )
    )
})
