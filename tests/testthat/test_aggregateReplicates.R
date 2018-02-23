context("aggregateReplicates")

test_that("Matrix", {
   data <- aggregateReplicates(mat, groupings = groupings)
   expect_is(data, "matrix")
   expect_identical(data, aggmat)
})

test_that("DgCMatrix", {
    dgc <- as(mat, "dgCMatrix")
    aggdgc <- as(aggmat, "dgCMatrix")
    data <- aggregateReplicates(dgc, groupings = groupings)
    expect_is(data, "dgCMatrix")
    expect_identical(data, aggdgc)
})

test_that("Invalid `groupings`", {
    expect_error(
        aggregateReplicates(mat, groupings = "XXX"),
        "is_factor"
    )
    expect_error(
        aggregateReplicates(mat, groupings = factor(c("XXX", "YYY"))),
        "are_identical"
    )
})
