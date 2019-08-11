context("pseudobulk")

test_that("SingleCellExperiment", {
    ## mean
    x <- pseudobulk(sce, fun = "mean")
    expect_identical(
        round(Matrix::colSums(counts(x))),
        c(sample1 = 61412, sample2 = 59478)  # nolint
    )
    ## geomean
    x <- pseudobulk(sce, fun = "geometricMean")
    expect_identical(
        round(Matrix::colSums(counts(x))),
        c(sample1 = 3576538, sample2 = 1943744)  # nolint
    )
    ## median
    x <- pseudobulk(sce, fun = "median")
    expect_identical(
        round(Matrix::colSums(counts(x))),
        c(sample1 = 3576538, sample2 = 1943744)  # nolint
    )
})
