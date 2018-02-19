context("assertIsAStringOrNULL")

test_that("Success", {
    expect_silent(assertIsAStringOrNULL("hello world"))
    expect_silent(assertIsAStringOrNULL(NULL))
})

test_that("Failure", {
    expect_error(
        assertIsAStringOrNULL(c("hello", "world")),
        "is_a_string : x has length 2, not 1."
    )
})
