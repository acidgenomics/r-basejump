context("headtail")

test_that("GRanges", {
    expect_output(
        object = headtail(gr),
        regexp = "ENSG00000000003"
    )
})

test_that("SummarizedExperiment", {
    expect_identical(
        capture.output(headtail(rse)),
        capture.output(headtail(assay(rse)))
    )
})
