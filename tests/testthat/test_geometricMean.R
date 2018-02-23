context("geometricMean")

test_that("Numeric", {
    geo1 <- round(geometricMean(vec1), digits = 6L)
    expect_identical(geo1, means[["vec1"]])
    geo2 <- round(geometricMean(vec2), digits = 6L)
    expect_identical(geo2, means[["vec2"]])
})

test_that("Column data", {
    data <- data.frame(vec1, vec2)
    geo1 <- round(geometricMean(data), digits = 6L)
    expect_identical(geo1, means)
    mat <- as.matrix(data)
    geo2 <- round(geometricMean(mat), digits = 6L)
    expect_identical(geo2, means)
})

test_that("Non-numeric column data", {
    expect_error(
        geometricMean(starwars),
        "is_numeric"
    )
})

test_that("NaN on negative numbers", {
    vec <- seq(from = -5L, to = 5L, by = 1L)
    expect_identical(geometricMean(vec), NaN)
})

test_that("Zero propagation", {
    vec <- seq(from = 0L, to = 5L, by = 1L)
    expect_identical(geometricMean(vec, zeroPropagate = TRUE), 0L)
    expect_identical(
        round(geometricMean(vec, zeroPropagate = FALSE), digits = 6L),
        2.220906
    )
})
