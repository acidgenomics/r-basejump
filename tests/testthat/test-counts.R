context("counts")

test_that("SummarizedExperiment", {
    object <- counts(rse)
    expect_is(object, "matrix")
})
