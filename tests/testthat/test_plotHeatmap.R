context("plotHeatmap")

test_that("matrix", {
    mat <- as.matrix(mtcars)
    p <- plotHeatmap(mat)
    expect_is(p, "list")
    expect_identical(
        names(p),
        c("tree_row", "tree_col", "kmeans", "gtable")
    )
})
