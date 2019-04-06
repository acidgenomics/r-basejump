context("sampleNames")

test_that("SummarizedExperiment", {
    object <- sampleNames(rse)
    expected <- as.character(sampleData(rse)[["sampleName"]])
    names(expected) <- colnames(rse)
    expect_identical(object, expected)
})

test_that("SE assignment", {
    samples <- letters[seq_len(ncol(rse))]
    names(samples) <- colnames(rse)
    sampleNames(rse) <- samples
    expect_s4_class(rse, "RangedSummarizedExperiment")
    expect_identical(sampleNames(rse), samples)
})

test_that("SE assignment failures", {
    expect_error(
        sampleNames(rse) <- "xxx",
        "The names of value are NULL."
    )
    expect_error(
        sampleNames(rse) <- c(aaa = "bbb"),
        "areSetEqual"
    )
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
