context("setAs")

test_that("Coerce to tibble", {
    x <- as(df, "tibble")
    expect_is(x, "tbl_df")
    # Expect that rownames are automatically moved to first column
    expect_identical(
        colnames(x)[[1L]],
        "rowname"
    )
    expect_error(
         as(list("foo", "bar"), "tibble"),
        "has_dims :"
    )
})
