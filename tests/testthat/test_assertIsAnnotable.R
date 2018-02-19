context("assertIsAnnotable")

test_that("Success", {
    annotable <- annotable("Homo sapiens", quiet = TRUE)
    expect_silent(assertIsAnnotable(annotable))
})

test_that("Failure", {
    expect_error(
        assertIsAnnotable(mtcars),
        "is_subset"
    )
})

test_that("Object isn't a data.frame", {
    expect_error(
        assertIsAnnotable(NULL),
        "is_data.frame"
    )
})
