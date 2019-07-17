context("zerosVsDepth")

test_that("SummarizedExperiment", {
    x <- zerosVsDepth(rse)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        round(mean(x[["dropout"]]), digits = 2L),
        0.09
    )
    expect_identical(
        round(mean(x[["depth"]]), digits = 2L),
        22543.33
    )
})

test_that("SingleCellExperiment", {
    x <- zerosVsDepth(sce)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        round(mean(x[["dropout"]]), digits = 2L),
        0.76
    )
    expect_identical(
        round(mean(x[["depth"]]), digits = 2L),
        245.41
    )
})
