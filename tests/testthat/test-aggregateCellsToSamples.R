context("aggregateCellsToSamples")

test_that("SingleCellExperiment", {
    object <- aggregateCellsToSamples(sce)
    counts <- head(counts(object))
    expected <- matrix(
        data = c(
            33L, 124L, 1658L, 2018L, 2320L, 10730L,
            12L,  58L,  728L,  982L, 1362L,  6357L
        ),
        nrow = 6L,
        ncol = 2L,
        dimnames = list(
            head(rownames(sce)),
            names(sampleNames(sce))
        )
    )
    expect_equal(as.matrix(counts), as.matrix(expected))
})
