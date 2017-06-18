context("Mathematical operations")

test_that("logRatioToFoldChange", {
    lr <- seq(-3L, 3L, 1L)
    fc <- c(-8L, -4L, -2L, 1L, 2L, 4L, 8L)
    expect_equal(lr2fc(lr), fc)
    expect_equal(fc2lr(fc), lr)
})
