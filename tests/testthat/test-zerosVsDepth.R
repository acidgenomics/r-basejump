context("zerosVsDepth")

test_that("SummarizedExperiment", {
    x <- zerosVsDepth(rse)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        object = round(mean(x[["dropout"]]), digits = 2L),
        expected = 0.1
    )
    expect_identical(
        round(mean(x[["depth"]]), digits = 2L),
        23805.83
    )
})

test_that("SingleCellExperiment", {
    x <- zerosVsDepth(sce)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        object = round(mean(x[["dropout"]]), digits = 2L),
        expected = 0.52
    )
    expect_identical(
        object = round(mean(x[["depth"]]), digits = 2L),
        expected = 55202.82
    )
})
