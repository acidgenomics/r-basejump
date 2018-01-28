context("plotQuantileHeatmap")

plotNames <- c("tree_row", "tree_col", "kmeans", "gtable")

test_that("Simple matrix", {
    mat <- matrix(1:10000, nrow = 100, ncol = 100)
    p <- plotQuantileHeatmap(mat)
    expect_identical(
        names(p),
        c("quantiles", "plot")
    )
    expect_identical(
        names(p[["plot"]]),
        plotNames
    )
})

test_that("Annotation columns support", {
    loadRemoteData("http://basejump.seq.cloud/counts.rda", quiet = TRUE)
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
    expect_identical(
        names(p[["plot"]]),
        plotNames
    )
    # TODO Add a method here to extract the annotation columns from the gtable
    # grobs. Not sure how to do this yet.
})
