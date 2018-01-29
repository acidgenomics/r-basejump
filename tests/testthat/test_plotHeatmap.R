context("plotHeatmap")

loadRemoteData("http://basejump.seq.cloud/counts.rda", quiet = TRUE)
plotNames <- c("tree_row", "tree_col", "kmeans", "gtable")

test_that("matrix", {
    mat <- as.matrix(mtcars)
    p <- plotHeatmap(mat)
    expect_is(p, "list")
    expect_identical(names(p), plotNames)
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
    p <- plotHeatmap(counts, annotationCol = annotationCol)
    expect_identical(names(p), plotNames)
    expect_length(p[["gtable"]], 9L)
})

test_that("Default color palette", {
    expect_silent(plotHeatmap(counts, color = NULL))
})

test_that("Turn off columns for many samples", {
    matrix <- matrix(seq(1L:1000L), ncol = 100L)
    p <- plotHeatmap(matrix)
    expect_identical(names(p), plotNames)
    expect_length(p[["gtable"]], 5L)
})

test_that("Matrix dimensions are too small", {
    expect_error(
        plotHeatmap(matrix(seq(1L:10L), ncol = 1L)),
        "Need at least 2 columns to plot heatmap"
    )
    expect_error(
        plotHeatmap(matrix(seq(1L:10L), nrow = 1L)),
        "Need at least 2 rows to plot heatmap"
    )
})
