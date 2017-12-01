context("geomean")

vec1 <- seq(from = 1L, to = 5L, by = 1L)
vec2 <- vec1 ^ 2L
means <- c(vec1 = 2.605171, vec2 = 6.786916)

test_that("numeric", {
    geo1 <- round(geomean(vec1), digits = 6L)
    expect_equal(geo1, means[["vec1"]])
    geo2 <- round(geomean(vec2), digits = 6L)
    expect_equal(geo2, means[["vec2"]])
})

test_that("Column data", {
    df <- data.frame(vec1, vec2)
    geo1 <- round(geomean(df), digits = 6L)
    expect_identical(geo1, means)
    mat <- as.matrix(df)
    geo2 <- round(geomean(mat), digits = 6L)
    expect_identical(geo2, means)
})

test_that("Non-numeric column data", {
    expect_error(
        geomean(starwars),
        paste("Non-numeric columns: name, hair_color,")
    )
})

test_that("NaN on negative numbers", {
    vec <- seq(from = -5L, to = 5L, by = 1L)
    expect_equal(
        geomean(vec),
        NaN
    )
})

test_that("Zero propagation", {
    vec <- seq(from = 0L, to = 5L, by = 1L)
    expect_equal(
        geomean(vec, zeroPropagate = TRUE),
        0L
    )
    expect_equal(
        round(geomean(vec, zeroPropagate = FALSE), digits = 6L),
        2.220906
    )
})
