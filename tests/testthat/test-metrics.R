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

## nolint start
## fun <- eval(formals(metricsPerSample.SingleCellExperiment)[["fun"]])
## nolint end

test_that("SingleCellExperiment", {
    object <- sce
    ## Simulate a metric column with an expected mean.
    ## Standard normal distribution
    colData(object)[["nUMI"]] <- Matrix::colSums(counts(object))
    expect_identical(
        object = metricsPerSample(object, fun = "mean") %>%
            .[["nUMI"]] %>%
            round(digits = 2L),
        expected = c(55883.41, 53992.89)
    )
    expect_identical(
        object = metricsPerSample(object, fun = "median") %>%
            .[["nUMI"]] %>%
            round(digits = 2L) %>%
            as.integer(),
        expected = c(54488L, 51660L)
    )
    expect_equal(
        object = metricsPerSample(object, fun = "sum") %>%
            .[["nUMI"]] %>%
            as.integer(),
        expected = c(3576538, 1943744L)
    )
})

test_that("Missing nUMI column", {
    expect_error(
        object = metricsPerSample(sce, fun = "sum"),
        expected = paste(
            "`sum` method only applies to metrics columns",
            "prefixed with `n` (e.g. `nUMI`)."
        )
    )
})
