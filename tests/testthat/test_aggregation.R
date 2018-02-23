context("Aggregation Utilities")

test_that("aggregateFeatures", {
    groupings <- factor(c("gene_1", "gene_1", "gene_2", "gene_2"))
    names(groupings) <- rownames(df)

    # matrix ===================================================================
    x <- aggregateFeatures(mat, groupings = groupings)
    expect_is(x, "matrix")
    expect_identical(x, matAggFeat)

    # dgCMatrix ================================================================
    x <- aggregateFeatures(dgc, groupings = groupings)
    y <- as(matAggFeat, "dgCMatrix")
    expect_is(x, "dgCMatrix")
    expect_identical(as.matrix(x), as.matrix(y))

    # Invalid groupings ========================================================
    expect_error(
        aggregateFeatures(mat, groupings = "XXX"),
        "is_factor"
    )
    expect_error(
        aggregateFeatures(mat, groupings = factor(c("XXX", "YYY"))),
        "are_identical"
    )
})

test_that("aggregateReplicates", {
    groupings <- factor(c("sample_1", "sample_1", "sample_2", "sample_2"))
    names(groupings) <- colnames(df)

    # matrix ===================================================================
    x <- aggregateReplicates(mat, groupings = groupings)
    expect_is(x, "matrix")
    expect_identical(x, matAggRep)

    # dgCMatrix ================================================================
    x <- aggregateReplicates(dgc, groupings = groupings)
    y <- as(matAggRep, "dgCMatrix")
    expect_is(x, "dgCMatrix")
    expect_identical(x, y)

    # Invalid groupings ========================================================
    expect_error(
        aggregateReplicates(mat, groupings = "XXX"),
        "is_factor"
    )
    expect_error(
        aggregateReplicates(mat, groupings = factor(c("XXX", "YYY"))),
        "are_identical"
    )
})
