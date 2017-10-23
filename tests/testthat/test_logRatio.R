context("logRatio")

test_that("logRatio", {
    fc <- c(-8L, -4L, -2L, 1L, 2L, 4L, 8L)
    lr <- seq(-3L, 3L, 1L)
    expect_equal(fc2lr(fc), lr)
    expect_equal(lr2fc(lr), fc)
})
