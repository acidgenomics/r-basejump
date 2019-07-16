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

test_that("SummarizedExperiment <-", {
    object <- rse
    sampleData(object)[["test"]] <- as.factor(seq_len(ncol(object)))
    expect_is(sampleData(object)[["test"]], "factor")
})

# Note that this doesn't sort the row names automatically. Here we're doing this
# internally in the check, to make the example "sce" object more resistant to
# code-breaking updates.
test_that("SingleCellExperiment", {
    samples <- paste0("sample", seq_len(2L))
    sd <- sampleData(sce)
    expect_identical(sort(rownames(sd)), samples)
    expect_identical(
        sd[samples, ],
        DataFrame(
            sampleName = as.factor(samples),
            interestingGroups = as.factor(samples),
            row.names = samples
        )
    )
})

test_that("SingleCellExperiment <-", {
    samples <- paste0("sample", seq_len(2L))
    batch <- factor(seq_along(samples))
    sampleData(sce)[["batch"]] <- batch
    expect_identical(
        object = sampleData(sce),
        expected = DataFrame(
            sampleName = as.factor(samples),
            batch = batch,
            interestingGroups = as.factor(samples),
            row.names = samples
        )
    )
})
