context("geometricMean")

test_that("geometricMean", {
    int <- seq(from = 1L, to = 5L, by = 1L)
    num <- int ^ 2L
    df <- data.frame(int, num)
    mat <- as.matrix(df)
    mean <- c(int = 2.605171, num = 6.786916)

    # integer ==================================================================
    expect_identical(
        round(geometricMean(int), digits = 6L),
        mean[["int"]]
    )

    # numeric ==================================================================
    expect_identical(
        round(geometricMean(num), digits = 6L),
        mean[["num"]]
    )

    # data.frame ===============================================================
    expect_identical(
        round(geometricMean(df), digits = 6L),
        mean
    )
    expect_error(
        geometricMean(starwars),
        "is_numeric :"
    )

    # matrix ===================================================================
    expect_identical(
        round(geometricMean(mat), digits = 6L),
        mean
    )

    # NaN on negative numbers ==================================================
    x <- seq(from = -5L, to = 5L, by = 1L)
    expect_identical(geometricMean(x), NaN)

    # Zero propagation =========================================================
    x <- seq(from = 0L, to = 5L, by = 1L)
    expect_identical(
        geometricMean(x, zeroPropagate = TRUE),
        0L
    )
    expect_identical(
        round(geometricMean(x, zeroPropagate = FALSE), digits = 6L),
        2.220906
    )
})
