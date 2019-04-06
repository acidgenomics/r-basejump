context("mcolnames")

test_that("DataFrame", {
    expect_identical(
        object = mcols(df),
        expected = NULL
    )
})

test_that("GRanges", {
    expect_identical(
        object = mcolnames(gr),
        expected = c("geneID", "geneName")
    )
})

test_that("SummarizedExperiment", {
    expect_identical(
        object = mcolnames(rse),
        expected = c(
            "geneID", "geneName", "geneBiotype", "broadClass", "entrezID"
        )
    )
})



context("mcolnames<-")

test_that("Don't allow length mismatch", {
    expect_error(
        mcolnames(df) <- "xxx",
        "colnames\\(mcols\\(x\\)\\) does not have the same length as value."
    )
})
