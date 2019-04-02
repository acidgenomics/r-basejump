context("aggregateRows")

expected <- as.matrix(DataFrame(
    "sample1" = c(3L, 7L),
    "sample2" = c(11L, 15L),
    "sample3" = c(19L, 23L),
    "sample4" = c(27L, 31L),
    row.names = c("gene1", "gene2")
))

groupings <- as.factor(paste0("gene", rep(seq_len(2L), each = 2L)))
names(groupings) <- rownames(mat)

test_that("matrix", {
    object <- aggregateRows(mat, groupings = groupings)
    expect_is(object, "matrix")
    expect_identical(object, expected)
})

test_that("sparseMatrix", {
    object <- aggregateRows(sparse, groupings = groupings)
    expect_is(object, "sparseMatrix")
    # Is there a way to improve this check?
    expect_equal(
        object = as.matrix(object),
        expected = expected
    )
})

test_that("Invalid groupings", {
    expect_error(
        object = aggregateRows(mat, groupings = "XXX"),
        regexp = "is.factor"
    )
    expect_error(
        object = aggregateRows(mat, groupings = factor(c("XXX", "YYY"))),
        regexp = "identical"
    )
})



context("aggregateCols")

expected <- as.matrix(DataFrame(
    "sample1" = c(6L, 8L, 10L, 12L),
    "sample2" = c(22L, 24L, 26L, 28L),
    row.names = rownames(mat)
))

groupings <- as.factor(paste0("sample", rep(seq_len(2L), each = 2L)))
names(groupings) <- colnames(df)

test_that("matrix", {
    object <- aggregateCols(mat, groupings = groupings)
    expect_is(object, "matrix")
    expect_identical(object, expected)
})

test_that("sparseMatrix", {
    object <- aggregateCols(sparse, groupings = groupings)
    expect_is(object, "sparseMatrix")
    # Is there a way to improve this check?
    expect_equal(
        object = as.matrix(object),
        expected = expected
    )
})

test_that("Invalid groupings", {
    expect_error(
        object = aggregateCols(mat, groupings = "XXX"),
        regexp = "is.factor"
    )
    expect_error(
        object = aggregateCols(mat, groupings = factor(c("XXX", "YYY"))),
        regexp = "identical"
    )
})
