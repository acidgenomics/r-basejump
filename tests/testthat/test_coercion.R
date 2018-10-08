context("Coercion")



# list =========================================================================
test_that("list : SummarizedExperiment", {
    object <- as(rse_small, "list")
    expect_is(object, "list")
    expect_identical(
        object = names(object),
        expected = c(
            "rowRanges",
            "colData",
            "assays",
            "NAMES",
            "elementMetadata",
            "metadata"
        )
    )

    # S4 coercion to list method support.
    expect_identical(
        object = as.list(rse_small),
        expected = as(rse_small, "list")
    )
})



# tbl_df =======================================================================
test_that("tbl_df", {
    x <- as(df, "tbl_df")
    expect_is(x, "tbl_df")

    # Expect that rownames are automatically moved to first column.
    expect_identical(colnames(x)[[1L]], "rowname")

    # Early return if already tibble.
    x <- tibble()
    expect_identical(x, as(x, "tbl_df"))
})
