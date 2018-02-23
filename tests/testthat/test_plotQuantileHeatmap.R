context("plotQuantileHeatmap")

test_that("Simple matrix", {
    mat <- matrix(1:10000, nrow = 100, ncol = 100)
    p <- plotQuantileHeatmap(mat)
    expect_is(p, "list")
    expect_identical(names(p), c("quantiles", "plot"))
    expect_identical(names(p[["plot"]]), plotNames)
    expect_length(p[["plot"]][["gtable"]], 5L)
})

test_that("Annotation columns support", {
    annotationCol <- data.frame(
        genotype = c(
            "wildtype",
            "wildtype",
            "mutant",
            "mutant"
        ),
        row.names = colnames(counts)
    )
    p <- plotQuantileHeatmap(counts, annotationCol = annotationCol)
    expect_identical(names(p[["plot"]]), plotNames)
    expect_length(p[["plot"]][["gtable"]], 9L)
})

test_that("Default color palette", {
    p <- plotQuantileHeatmap(counts, color = NULL)
    expect_is(p, "list")
})

test_that("Matrix dimensions are too small", {
    expect_error(
        plotQuantileHeatmap(matrix(seq(1L:10L), ncol = 1L)),
        "is_greater_than : ncol"
    )
    expect_error(
        plotQuantileHeatmap(matrix(seq(1L:10L), nrow = 1L)),
        "is_greater_than : nrow"
    )
})
