context("pseudobulk")

object <- sce

test_that("mean", {
    object <- pseudobulk(object, fun = "mean")
    object <- round(Matrix::colSums(counts(object)))
    expected <- c(sample1 = 62958, sample2 = 61262)  # nolint
    expect_identical(object, expected)
})

test_that("geomean", {
    object <- pseudobulk(object, fun = "geometricMean")
    object <- round(Matrix::colSums(counts(x)))
    expected <- c(sample1 = 62958, sample2 = 61262)  # nolint
    expect_identical(object, expected)
})

test_that("median", {
    object <- pseudobulk(object, fun = "median")
    object <- round(Matrix::colSums(counts(object)))
    expected <- c(sample1 = 2794849, sample2 = 2813670)  # nolint
    expect_identical(object, expected)
})
