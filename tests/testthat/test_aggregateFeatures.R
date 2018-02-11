context("aggregateFeatures")

mat <- data.frame(
    "sample1" = c(0L, 1L, 2L, 3L),
    "sample2" = c(1L, 2L, 3L, 4L),
    row.names = c("gene1.1", "gene1.2", "gene2.1", "gene2.2")) %>%
    as.matrix()
aggmat <- data.frame(
    "sample1" = c(1L, 5L),
    "sample2" = c(3L, 7L),
    row.names = c("gene1", "gene2")) %>%
    as.matrix()

groupings <- factor(c("gene1", "gene1", "gene2", "gene2"))
names(groupings) <- rownames(mat)

test_that("matrix", {
   data <- aggregateFeatures(mat, groupings = groupings)
   expect_is(data, "matrix")
   expect_identical(data, aggmat)
})

test_that("dgCMatrix", {
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
        paste(
            "is_factor :",
            "groupings is not of class 'factor';",
            "it has class 'character'."
        )
    )
    expect_error(
        aggregateFeatures(mat, groupings = factor(c("XXX", "YYY"))),
        paste(
            "are_identical :",
            "rownames\\(object\\) and names\\(groupings\\) are not identical."
        )
    )
})
