context("foldChange and logRatio")

fc <- c(-8L, -4L, -2L, 1L, 2L, 4L, 8L)
lr <- seq(-3L, 3L, 1L)

test_that("foldChangeToLogRatio", {
    expect_equal(foldChangeToLogRatio(fc), lr)
    expect_error(
        foldChangeToLogRatio(lr, base = 0L),
        "is_positive : base"
    )
})

test_that("logRatioToFoldChange", {
    expect_equal(logRatioToFoldChange(lr), fc)
    expect_error(
        logRatioToFoldChange(lr, base = 0L),
        "is_positive : base"
    )
})


