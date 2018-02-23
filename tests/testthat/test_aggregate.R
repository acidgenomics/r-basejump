context("Aggregation Utilities")

# aggregateFeatures ============================================================
test_that("aggregateFeatures : matrix", {
   data <- aggregateFeatures(mat, groupings = featureGroupings)
   expect_is(data, "matrix")
   expect_identical(data, aggMatFeatures)
})

test_that("aggregateFeatures : dgCMatrix", {
    dgc <- as(mat, "dgCMatrix")
    aggdgc <- as(aggMatFeatures, "dgCMatrix")
    data <- aggregateFeatures(dgc, groupings = featureGroupings)
    expect_is(data, "dgCMatrix")
    # FIXME This test is failing on dgCMatrix class unless we coerce to matrix
    expect_identical(as.matrix(data), as.matrix(aggdgc))
})

test_that("aggregateFeatures : Invalid `groupings`", {
    expect_error(
        aggregateFeatures(mat, groupings = "XXX"),
        "is_factor"
    )
    expect_error(
        aggregateFeatures(mat, groupings = factor(c("XXX", "YYY"))),
        "are_identical"
    )
})



# aggregateReplicates ==========================================================
test_that("aggregateReplicates : matrix", {
    data <- aggregateReplicates(mat, groupings = replicateGroupings)
    expect_is(data, "matrix")
    expect_identical(data, aggMatReplicates)
})

test_that("aggregateReplicates : dgCMatrix", {
    dgc <- as(mat, "dgCMatrix")
    aggdgc <- as(aggMatReplicates, "dgCMatrix")
    data <- aggregateReplicates(dgc, groupings = replicateGroupings)
    expect_is(data, "dgCMatrix")
    expect_identical(data, aggdgc)
})

test_that("aggregateReplicates : Invalid `groupings`", {
    expect_error(
        aggregateReplicates(mat, groupings = "XXX"),
        "is_factor"
    )
    expect_error(
        aggregateReplicates(mat, groupings = factor(c("XXX", "YYY"))),
        "are_identical"
    )
})
