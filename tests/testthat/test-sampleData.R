context("sampleData")

test_that("SummarizedExperiment", {
    object <- rse
    # Check that `sampleName` and `interestingGroups` auto-populate.
    expect_identical(
        object = setdiff(
            x = colnames(sampleData(object)),
            y = colnames(colData(object))
        ),
        expected = c("sampleName", "interestingGroups")
    )
    expect_identical(
        object = sampleData(object)[, colnames(colData(object)), drop = FALSE],
        expected = colData(object)
    )

    # Empty `colData` is supported. Changed in v0.99.
    object <- rse
    colData(object) <- DataFrame(row.names = colnames(object))
    interestingGroups(object) <- NULL
    expect_silent(sampleData(object))
})

test_that("SummarizedExperiment assignment method", {
    object <- rse
    sampleData(object)[["test"]] <- as.factor(seq_len(ncol(object)))
    expect_is(sampleData(object)[["test"]], "factor")
})
