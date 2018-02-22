context("assertIsAnnotable")

test_that("Success", {
    annotable <- annotable("Homo sapiens")
    expect_silent(assertIsAnnotable(annotable))
})

test_that("Failure", {
    expect_error(
        assertIsAnnotable(mtcars),
        "is_subset : "
    )
    expect_error(
        assertIsAnnotable(NULL),
        "is_data.frame : x"
    )
})
