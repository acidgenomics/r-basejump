context("meltCounts")

test_that("SummarizedExperiment", {
    x <- meltCounts(rse)
    expect_s3_class(x, "tbl_df")
})

test_that("Non-zero features (genes)", {
    a <- meltCounts(rse, minCounts = 0L)
    expect_true(any(a[["counts"]] == 0L))

    b <- meltCounts(rse, minCounts = 1L)
    # Note that this step shouldn't drop all zeros, only all-zero genes.
    expect_true(any(b[["counts"]] == 0L))

    # Check for removal of our all-zero gene.
    expect_identical(
        object = setdiff(a[["rowname"]], b[["rowname"]]),
        expected = "gene433"
    )
    expect_true(all(assay(rse)["gene433", , drop = TRUE] == 0L))
})

trans <- eval(formals(meltCounts.SummarizedExperiment)[["trans"]])
with_parameters_test_that(
    "trans (log transformations)", {
        x <- meltCounts(rse, trans = trans)
        expect_s3_class(x, "tbl_df")
        expect_identical(
            object = round(head(x[["counts"]]), digits = 3L),
            expected = expected
        )
    },
    trans = trans,
    expected = list(
        identity = c(1, 30, 27, 253, 0, 1),  # nolint
        log2 = c(1.000, 4.954, 4.807, 7.989, 0.000, 1.000),
        log10 = c(0.301, 1.491, 1.447, 2.405, 0.000, 0.301)
    )
)
