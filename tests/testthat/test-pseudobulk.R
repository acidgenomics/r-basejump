context("pseudobulk")

object <- sce

test_that("sum", {
    object <- pseudobulk(object, fun = "sum")
    object <- round(Matrix::colSums(counts(object)))
    expected <- c(sample1 = 2794849, sample2 = 2813670)  # nolint
    expect_identical(object, expected)
})

# FIXME Now this is returning NaN due to zeros, uh oh.
test_that("mean", {
    object <- pseudobulk(object, fun = "mean")
    object <- round(Matrix::colSums(counts(object)))
    expected <- c(sample1 = 62958, sample2 = 61262)  # nolint
    expect_identical(object, expected)
})
