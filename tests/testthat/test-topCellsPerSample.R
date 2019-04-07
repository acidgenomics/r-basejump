context("topCellsPerSample")

test_that("SingleCellExperiment", {
    x <- topCellsPerSample(sce, n = 5L)
    expect_is(x, "list")
    expect_identical(
        object = x,
        expected = list(
            sample1 = c("cell042", "cell063", "cell009", "cell094", "cell057"),
            sample2 = c("cell005", "cell074", "cell076", "cell067", "cell018")
        )
    )
})
