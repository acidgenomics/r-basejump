context("headtail")

test_that("atomic", {
    expect_identical(
        object = capture.output(headtail(LETTERS)),
        expected = "A B ... Y Z"
    )

})

test_that("matrix", {
    mat <- assay(rse)
    output <- capture.output(headtail(mat))
    expect_true(grepl("gene001", output[[2L]]))
})

test_that("matrix : No quadrants", {
    expect_message(
        object = capture.output(headtail(mat)),
        regexp = "Object can't be split into quadrants."
    )
})

test_that("GRanges", {
    output <- capture.output(headtail(gr))
    expect_true(grepl("ENSG00000000003", output[[2L]]))
})

test_that("SummarizedExperiment", {
    expect_identical(
        capture.output(headtail(rse)),
        capture.output(headtail(assay(rse)))
    )
})

test_that("ASCII mode", {
    output <- capture.output(headtail(rse, ascii = TRUE))
    expect_true(grepl("...", output[[1L]]))
})
