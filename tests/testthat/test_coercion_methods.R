context("Coercion Methods")



# tibble =======================================================================
test_that("tibble : data.frame", {
    x <- as(df, "tbl_df")
    expect_is(x, "tbl_df")

    # Expect that rownames are automatically moved to first column
    expect_identical(
        colnames(x)[[1L]],
        "rowname"
    )

    # Also support use of `tibble` instead of `tbl_df`
    y <- as(df, "tibble")
    expect_identical(x, y)
})

test_that("tibble : tbl_df (unmodified)", {
    x <- tibble()
    expect_identical(x, as(x, "tbl_df"))
})
