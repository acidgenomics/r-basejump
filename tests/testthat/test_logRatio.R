context("foldChange and logRatio")

fc <- c(-8L, -4L, -2L, 1L, 2L, 4L, 8L)
lr <- seq(-3L, 3L, 1L)

test_that("foldChangeToLogRatio", {
    expect_equal(foldChangeToLogRatio(fc), lr)
})

test_that("logRatioToFoldChange", {
    expect_equal(logRatioToFoldChange(lr), fc)
})
