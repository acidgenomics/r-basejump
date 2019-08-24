## FIXME Rename to gather.

context("meltCounts")

test_that("SummarizedExperiment", {
    x <- meltCounts(rse)
    expect_s4_class(x, "DataFrame")
})

## Here we're checking the handling of zero count genes.
test_that("minCounts", {
    object <- rse
    assay(object)[seq_len(2L), ] <- 0L
    x <- meltCounts(object, minCounts = 1L)

    ## Check for removal of our all-zero gene.
    expect_identical(
        object = setdiff(x = rownames(object), y = unique(x[["rowname"]])),
        expected = head(rownames(object), n = 2L)
    )
    ## Note that this step shouldn't drop all zeros, only all-zero genes.
    expect_true(any(x[["counts"]] == 0L))
})

test_that("Require at least 1 count per feature", {
    expect_error(
        object = meltCounts(rse, minCounts = 0L),
        regexp = "less than 1"
    )
})

trans <- eval(formals(`meltCounts,SummarizedExperiment`)[["trans"]])
with_parameters_test_that(
    "trans", {
        x <- meltCounts(rse, trans = trans)
        expect_s4_class(x, "DataFrame")
        expect_identical(
            object = round(head(x[["counts"]]), digits = 3L),
            expected = expected
        )
    },
    trans = trans,
    expected = list(
        identity = c(4, 49, 73, 0, 6, 12),  # nolint
        log2 = c(2.322, 5.644, 6.209, 0.000, 2.807, 3.700),
        log10 = c(0.699, 1.699, 1.869, 0.000, 0.845, 1.114)
    )
)
