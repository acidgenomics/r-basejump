context("sampleNames")

test_that("SummarizedExperiment", {
    object <- sampleNames(rse)
    expected <- as.character(sampleData(rse)[["sampleName"]])
    names(expected) <- colnames(rse)
    expect_identical(object, expected)
})

test_that("sampleName column isn't factor", {
    colData(rse)[["sampleName"]] <- "xxx"
    expect_error(
        object = sampleNames(rse),
        expected = paste(
            "sampleData() requires a `sampleName`",
            "factor column in colData()."
        )
    )
})
