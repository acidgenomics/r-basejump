context("mapCellsToSamples")

test_that("mapCellsToSamples", {
    samples <- paste0("sample", seq_len(2L))
    cells <- paste(samples, c("AAAAAAAA", "CCCCCCCC"), sep = "_")
    x <- mapCellsToSamples(cells, samples)
    expect_is(x, "factor")
    expect_identical(
        object = x,
        expected = as.factor(c(
            sample1_AAAAAAAA = "sample1",
            sample2_CCCCCCCC = "sample2"
        ))
    )
})
