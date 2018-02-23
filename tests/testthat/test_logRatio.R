context("logRatio")

test_that("logRatio", {
    fc <- c(-8L, -4L, -2L, 1L, 2L, 4L, 8L)
    lr <- seq(-3L, 3L, 1L)
    vec1 <- seq(from = 1L, to = 5L, by = 1L)
    vec2 <- vec1 ^ 2L
    means <- c(vec1 = 2.605171, vec2 = 6.786916)
    expect_equal(foldChangeToLogRatio(fc), lr)
    expect_equal(logRatioToFoldChange(lr), fc)
    expect_error(
        foldChangeToLogRatio(lr, base = 0L),
        "is_positive : base"
    )
    expect_error(
        logRatioToFoldChange(lr, base = 0L),
        "is_positive : base"
    )
})
