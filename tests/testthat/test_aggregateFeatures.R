context("aggregateFeatures")

test_that("Matrix", {
   data <- aggregateFeatures(mat, groupings = groupings)
   expect_is(data, "matrix")
   expect_identical(data, aggmat)
})

test_that("DgCMatrix", {
    dgc <- as(mat, "dgCMatrix")
    aggdgc <- as(aggmat, "dgCMatrix")
    data <- aggregateFeatures(dgc, groupings = groupings)
    expect_is(data, "dgCMatrix")
    # This test is failing on dgCMatrix class
    expect_identical(
        as.matrix(data),
        as.matrix(aggdgc)
    )
})

test_that("Invalid `groupings`", {
    expect_error(
        aggregateFeatures(mat, groupings = "XXX"),
        "is_factor"
    )
    expect_error(
        aggregateFeatures(mat, groupings = factor(c("XXX", "YYY"))),
        "are_identical"
    )
})
