context("setAs")

test_that("Coerce to tibble", {
    data <- as(mtcars, "tibble")
    expect_is(data, "tbl_df")
    # Expect that rownames are automatically moved to first column
    expect_identical(colnames(data)[[1L]], "rowname")
    expect_error(
         as(list("foo", "bar"), "tibble"),
        "has_dims"
    )
})
