context("aggregateReplicates")

rownames <- c("gene1", "gene2", "gene3", "gene4", "gene5")
counts <- data.frame(
    "sample1_rep1" = c(0, 0, 0, 1.1, 2.2),
    "sample1_rep2" = c(0, 0, 0, 3.3, 4.4),
    "sample2_rep1" = c(1.1, 2.2, 0, 0, 0),
    "sample2_rep2" = c(3.3, 4.4, 0, 0, 0),
    row.names = rownames) %>%
    as.matrix()
aggcounts <- data.frame(
    "sample1" = c(0, 0, 0, 4.4, 6.6),
    "sample2" = c(4.4, 6.6, 0, 0, 0),
    row.names = rownames) %>%
    as.matrix()

groupings <- factor(c("sample1", "sample1", "sample2", "sample2"))
names(groupings) <- colnames(counts)

test_that("matrix", {
   data <- aggregateReplicates(mat, groupings = groupings)
   expect_is(data, "matrix")
   expect_equal(data, aggcounts)
})

test_that("dgCMatrix", {
    dgc <- as(mat, "dgCMatrix")
    aggdgc <- as(aggcounts, "dgCMatrix")
    data <- aggregateReplicates(dgc, groupings = groupings)
    expect_is(data, "dgCMatrix")
    expect_equal(data, aggdgc)
})
