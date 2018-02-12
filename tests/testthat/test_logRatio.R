context("foldChange and logRatio")

fc <- c(-8L, -4L, -2L, 1L, 2L, 4L, 8L)
lr <- seq(-3L, 3L, 1L)

test_that("foldChangeToLogRatio", {
    expect_identical(foldChangeToLogRatio(fc), lr)
})

test_that("logRatioToFoldChange", {
    expect_identical(logRatioToFoldChange(lr), fc)
})
