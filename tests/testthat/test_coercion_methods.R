context("Coercion Methods")



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
