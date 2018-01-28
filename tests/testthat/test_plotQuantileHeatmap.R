context("plotQuantileHeatmap")

test_that("Simple matrix", {
    mat <- matrix(1:10000, nrow = 100, ncol = 100)
    p <- plotQuantileHeatmap(mat)
    expect_identical(
        names(p),
        c("quantiles", "plot")
    )
    expect_identical(
        names(p[["plot"]]),
        c("tree_row", "tree_col", "kmeans", "gtable")
    )
})
