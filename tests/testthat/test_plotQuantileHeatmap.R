context("plotQuantileHeatmap")

test_that("Simple matrix", {
    mat <- matrix(1:10000, nrow = 100, ncol = 100)
    p <- plotQuantileHeatmap(mat)
})
