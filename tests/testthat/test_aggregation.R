context("Aggregation Utilities")

test_that("aggregateFeatures", {
    aggMat <- data.frame(
        "sample_1" = c(3L, 7L),
        "sample_2" = c(11L, 15L),
        "sample_3" = c(19L, 23L),
        "sample_4" = c(27L, 31L),
        row.names = c("gene_1", "gene_2")
    ) %>%
        as.matrix()

    groupings <- factor(c("gene_1", "gene_1", "gene_2", "gene_2"))
    names(groupings) <- rownames(df)

    # matrix ===================================================================
    x <- aggregateFeatures(mat, groupings = groupings)
    expect_is(x, "matrix")
    expect_identical(x, aggMat)

    # dgCMatrix ================================================================
    x <- aggregateFeatures(dgc, groupings = groupings)
    y <- as(aggMat, "dgCMatrix")
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
    aggMat <- data.frame(
        "sample_1" = c(6L, 8L, 10L, 12L),
        "sample_2" = c(22L, 24L, 26L, 28L),
        row.names = c("gene_1", "gene_2", "gene_3", "gene_4")
    ) %>%
        as.matrix()

    groupings <- factor(c("sample_1", "sample_1", "sample_2", "sample_2"))
    names(groupings) <- colnames(df)

    # matrix ===================================================================
    x <- aggregateReplicates(mat, groupings = groupings)
    expect_is(x, "matrix")
    expect_identical(x, aggMat)

    # dgCMatrix ================================================================
    x <- aggregateReplicates(dgc, groupings = groupings)
    y <- as(aggMat, "dgCMatrix")
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
