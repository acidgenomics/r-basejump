assay <- matrix(
    data = c(
        0L, 1L, 1L, 1L,
        1L, 0L, 1L, 1L,
        1L, 1L, 0L, 1L,
        1L, 1L, 1L, 0L
    ),
    nrow = 4L,
    ncol = 4L,
    byrow = TRUE,
    dimnames = list(
        paste0("transcript", seq_len(4L)),
        paste(
            paste0("sample", rep(seq_len(2L), each = 2L)),
            paste0("replicate", rep(seq_len(2L), times = 2L)),
            sep = "_"
        )
    )
)

sparse <- as(assay, "sparseMatrix")

genes <- factor(paste0("gene", rep(seq_len(2L), each = 2L)))
names(genes) <- rownames(assay)

samples <- factor(paste0("sample", rep(seq_len(2L), each = 2L)))
names(samples) <- colnames(assay)

se <- SummarizedExperiment(
    assay = list(assay = assay),
    colData = DataFrame(
        sampleName = as.factor(names(samples)),
        aggregate = samples
    ),
    rowData = DataFrame(aggregate = genes)
)



context("aggregateRows")

expected <- matrix(
    data = c(
        1L, 1L, 2L, 2L,
        2L, 2L, 1L, 1L
    ),
    nrow = 2L,
    ncol = 4L,
    byrow = TRUE,
    dimnames = list(
        levels(genes),
        colnames(assay)
    )
)

test_that("matrix", {
    object <- aggregateRows(assay, groupings = genes)
    expect_is(object, "matrix")
    expect_identical(object, expected)
})

test_that("matrix : acidtest::mat", {
    groupings <- as.factor(paste0("gene", rep(seq_len(2L), each = 2L)))
    names(groupings) <- rownames(mat)

    expected <- matrix(
        data = c(
            3L, 11L, 19L, 27L,
            7L, 15L, 23L, 31L
        ),
        nrow = 2L,
        ncol = 4L,
        byrow = TRUE,
        dimnames = list(
            levels(groupings),
            colnames(mat)
        )
    )

    object <- aggregateRows(mat, groupings = groupings)
    expect_is(object, "matrix")
    expect_identical(object, expected)
})

test_that("sparseMatrix", {
    object <- aggregateRows(sparse, groupings = genes)
    expect_is(object, "sparseMatrix")
    expect_equal(as.matrix(object), as.matrix(expected))
})

test_that("SummarizedExperiment", {
    object <- aggregateRows(se)
    expect_s4_class(object, "SummarizedExperiment")
    expect_identical(assay(object), expected)
})

test_that("Invalid groupings", {
    expect_error(
        object = aggregateRows(assay, groupings = "XXX"),
        regexp = "is.factor"
    )
    expect_error(
        object = aggregateRows(assay, groupings = factor(c("XXX", "YYY"))),
        regexp = "identical"
    )
})



context("aggregateCols")

expected <- matrix(
    data = c(
        1L, 1L, 2L, 2L,
        2L, 2L, 1L, 1L
    ),
    nrow = 4L,
    ncol = 2L,
    byrow = FALSE,
    dimnames = list(
        rownames(assay),
        levels(samples)
    )
)

test_that("matrix", {
    object <- aggregateCols(assay, groupings = samples)
    expect_is(object, "matrix")
    expect_identical(object, expected)
})

test_that("matrix : acidtest::mat", {
    groupings <- as.factor(paste0("sample", rep(seq_len(2L), each = 2L)))
    names(groupings) <- colnames(mat)

    expected <- matrix(
        data = c(
             6L,  8L, 10L, 12L,
            22L, 24L, 26L, 28L
        ),
        nrow = 4L,
        ncol = 2L,
        byrow = FALSE,
        dimnames = list(
            rownames(mat),
            levels(groupings)
        )
    )

    object <- aggregateCols(mat, groupings = groupings)
    expect_is(object, "matrix")
    expect_identical(object, expected)
})

test_that("sparseMatrix", {
    object <- aggregateCols(sparse, groupings = samples)
    expect_is(object, "sparseMatrix")
    ## Is there a way to improve this check?
    expect_equal(
        object = as.matrix(object),
        expected = expected
    )
})

test_that("SummarizedExperiment", {
    object <- aggregateCols(se)
    expect_s4_class(object, "SummarizedExperiment")
    expect_identical(assay(object), expected)
})

test_that("matrix : Invalid groupings", {
    expect_error(
        object = aggregateCols(assay, groupings = "XXX"),
        regexp = "is.factor"
    )
    expect_error(
        object = aggregateCols(assay, groupings = factor(c("XXX", "YYY"))),
        regexp = "identical"
    )
})
