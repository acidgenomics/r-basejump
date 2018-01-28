context("plotHeatmap")

plotNames <- c("tree_row", "tree_col", "kmeans", "gtable")

test_that("matrix", {
    mat <- as.matrix(mtcars)
    p <- plotHeatmap(mat)
    expect_is(p, "list")
    expect_identical(names(p), plotNames)
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
    p <- plotHeatmap(counts, annotationCol = annotationCol)
    expect_identical(names(p), plotNames)
    # TODO Add a method here to extract the annotation columns from the gtable
    # grobs. Not sure how to do this yet.
})
