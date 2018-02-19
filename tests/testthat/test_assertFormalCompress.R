context("assertFormalCompress")

test_that("Success", {
    expect_silent(assertFormalCompress("xz"))
})

test_that("Failure", {
    expect_error(
        assertFormalCompress("XXX"),
        paste(
            "is_subset :",
            "The element 'XXX' in x is not in",
            "c\\(\"bzip2\", \"gzip\", \"xz\"\\)."
        )
    )
})
