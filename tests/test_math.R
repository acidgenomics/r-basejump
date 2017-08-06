context("Math Utilities")

test_that("geomean", {
    # Vectors
    vec <- seq(1L, 5L, 1L)
    expect_equal(
        round(geomean(vec), digits = 6L),
        2.605171)
    vec2 <- vec ^ 2L
    expect_equal(
        round(geomean(vec2), digits = 6L),
        6.786916)

    # Column data
    means <- c(vec = 2.605171, vec2 = 6.786916)
    df <- data.frame(vec, vec2)
    expect_identical(round(geomean(df), digits = 6L), means)
    mat <- as.matrix(df)
    expect_identical(round(geomean(mat), digits = 6L), means)
})



test_that("logRatio", {
    fc <- c(-8L, -4L, -2L, 1L, 2L, 4L, 8L)
    lr <- seq(-3L, 3L, 1L)
    expect_equal(fc2lr(fc), lr)
    expect_equal(lr2fc(lr), fc)
})



test_that("percentage", {
    expect_equal(pct(0.1), "10.0%")
})
