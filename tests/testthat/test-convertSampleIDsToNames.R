context("convertSampleIDsToNames")

test_that("SummarizedExperiment", {
    newNames <- letters[seq_len(ncol(rse))]
    colData(rse)[["sampleName"]] <- as.factor(newNames)
    x <- convertSampleIDsToNames(rse)
    expect_identical(colnames(x), newNames)
})

test_that("SingleCellExperiment", {
    expect_message(
        convertSampleIDsToNames(sce),
        "Returning with column names unmodified."
    )
})
