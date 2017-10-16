context("geomean")

vec1 <- seq(1L, 5L, 1L)
vec2 <- vec1 ^ 2L
means <- c(vec1 = 2.605171, vec2 = 6.786916)

test_that("numeric", {
    geo1 <- round(geomean(vec1), digits = 6L)
    expect_equal(geo1, means[["vec1"]])
    geo2 <- round(geomean(vec2), digits = 6L)
    expect_equal(geo2, means[["vec2"]])
})

test_that("column data", {
    df <- data.frame(vec1, vec2)
    geo1 <- round(geomean(df), digits = 6L)
    expect_identical(geo1, means)
    mat <- as.matrix(df)
    geo2 <- round(geomean(mat), digits = 6L)
    expect_identical(geo2, means)
})
