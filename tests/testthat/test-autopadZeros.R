context("autopadZeros")

test_that("character", {
    expect_identical(
        object = autopadZeros(c("A1", "B10")),
        expected = c("A01", "B10")
    )
    expect_identical(
        object = autopadZeros(c("A1", "B10", "C100")),
        expected = c("A001", "B010", "C100")
    )
})

test_that("SummarizedExperiment", {
    x <- autopadZeros(
        object = rse,
        rownames = TRUE,
        colnames = TRUE,
        sort = TRUE
    )
    expect_s4_class(x, "RangedSummarizedExperiment")
})

test_that("SingleCellExperiment", {
    x <- autopadZeros(
        object = sce,
        rownames = TRUE,
        colnames = TRUE,
        sort = TRUE
    )
    expect_s4_class(x, "SingleCellExperiment")
})
