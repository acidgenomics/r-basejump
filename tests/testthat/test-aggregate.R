counts <- matrix(
    data = c(
        0L, 2L, 2L, 2L,
        2L, 0L, 2L, 2L,
        2L, 2L, 0L, 2L,
        2L, 2L, 2L, 0L
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

sparse <- as(counts, "sparseMatrix")

genes <- factor(paste0("gene", rep(seq_len(2L), each = 2L)))
names(genes) <- rownames(counts)

samples <- factor(paste0("sample", rep(seq_len(2L), each = 2L)))
names(samples) <- colnames(counts)

se <- SummarizedExperiment(
    assays = SimpleList(counts = counts),
    colData = DataFrame(
        sampleName = as.factor(names(samples)),
        aggregate = samples
    ),
    rowData = DataFrame(aggregate = genes)
)



context("aggregate")

test_that("'n' count mode", {
    object <- aggregate(counts, by = genes, fun = "n")
    expect_is(object, "matrix")
    expect_true(all(colSums(object) == 3L))
    object <- aggregate(sparse, by = genes, fun = "n")
    expect_is(object, "Matrix")
    expect_true(all(colSums(object) == 3L))
})



context("aggregateRows")

expected <- matrix(
    data = c(
        2L, 2L, 4L, 4L,
        4L, 4L, 2L, 2L
    ),
    nrow = 2L,
    ncol = 4L,
    byrow = TRUE,
    dimnames = list(
        levels(genes),
        colnames(counts)
    )
)

test_that("matrix", {
    object <- aggregateRows(counts, by = genes)
    expect_is(object, "matrix")
    expect_identical(object, expected)
})

test_that("matrix : AcidTest example", {
    by <- as.factor(paste0("gene", rep(seq_len(2L), each = 2L)))
    names(by) <- rownames(mat)
    object <- aggregateRows(mat, by = by)
    expect_is(object, "matrix")
    expected <- matrix(
        data = c(
             6L,  8L, 10L, 12L,
            22L, 24L, 26L, 28L
        ),
        nrow = 2L,
        ncol = 4L,
        byrow = TRUE,
        dimnames = list(
            levels(by),
            colnames(mat)
        )
    )
    expect_identical(object, expected)
})

test_that("sparseMatrix", {
    object <- aggregateRows(sparse, by = genes)
    expect_is(object, "sparseMatrix")
    expect_equal(as.matrix(object), as.matrix(expected))
})

## Now requiring "counts" assay to be defined, as of v0.11.4.
test_that("SummarizedExperiment", {
    object <- aggregateRows(se)
    expect_s4_class(object, "SummarizedExperiment")
    expect_identical(assayNames(object), "counts")
    expect_identical(counts(object), expected)
})

test_that("Invalid groupings", {
    expect_error(
        object = aggregateRows(counts, by = "XXX"),
        regexp = "is.factor"
    )
    expect_error(
        object = aggregateRows(counts, by = factor(c("XXX", "YYY"))),
        regexp = "identical"
    )
})



context("aggregateCols")

expected <- matrix(
    data = c(
        2L, 2L, 4L, 4L,
        4L, 4L, 2L, 2L
    ),
    nrow = 4L,
    ncol = 2L,
    byrow = FALSE,
    dimnames = list(
        rownames(counts),
        levels(samples)
    )
)

test_that("matrix", {
    object <- aggregateCols(counts, by = samples)
    expect_is(object, "matrix")
    expect_identical(object, expected)
})

test_that("matrix : AcidTest example", {
    by <- as.factor(paste0("sample", rep(seq_len(2L), each = 2L)))
    names(by) <- colnames(mat)
    object <- aggregateCols(mat, by = by)
    expect_is(object, "matrix")
    expected <- matrix(
        data = c(
            3L, 11L, 19L, 27L,
            7L, 15L, 23L, 31L
        ),
        nrow = 4L,
        ncol = 2L,
        byrow = FALSE,
        dimnames = list(
            rownames(mat),
            levels(by)
        )
    )
    expect_identical(object, expected)
})

test_that("sparseMatrix", {
    object <- aggregateCols(sparse, by = samples)
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
    expect_identical(assayNames(object), "counts")
    expect_identical(counts(object), expected)
})

test_that("matrix : Invalid groupings", {
    expect_error(
        object = aggregateCols(counts, by = "XXX"),
        regexp = "is.factor"
    )
    expect_error(
        object = aggregateCols(counts, by = factor(c("XXX", "YYY"))),
        regexp = "identical"
    )
})
