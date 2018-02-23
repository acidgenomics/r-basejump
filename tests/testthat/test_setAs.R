context("setAs")

test_that("Coerce to tibble", {
    tbl <- as(data, "tibble")
    expect_is(tbl, "tbl_df")
    # Expect that rownames are automatically moved to first column
    expect_identical(colnames(tbl)[[1L]], "rowname")
    expect_error(
         as(list("foo", "bar"), "tibble"),
        "has_dims"
    )
})
