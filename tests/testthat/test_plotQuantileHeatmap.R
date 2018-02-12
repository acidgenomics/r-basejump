context("plotQuantileHeatmap")

loadRemoteData("http://basejump.seq.cloud/counts.rda", quiet = TRUE)
plotNames <- c("tree_row", "tree_col", "kmeans", "gtable")

test_that("Simple matrix", {
    mat <- matrix(1:10000, nrow = 100, ncol = 100)
    expect_error(
        plotQuantileHeatmap(mat),
        "has_colnames"
    )
    colnames(mat) <- paste0("col", seq_len(ncol(mat)))
    p <- plotQuantileHeatmap(mat)
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
    expect_silent(plotQuantileHeatmap(counts, color = NULL))
})

test_that("Matrix dimensions are too small", {
    expect_error(
        plotQuantileHeatmap(matrix(seq(1L:10L), ncol = 1L)),
        "Need at least 2 columns to plot heatmap"
    )
    expect_error(
        plotQuantileHeatmap(matrix(seq(1L:10L), nrow = 1L)),
        "Need at least 2 rows to plot heatmap"
    )
})
