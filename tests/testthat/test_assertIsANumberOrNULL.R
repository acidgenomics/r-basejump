context("assertIsANumberOrNULL")

test_that("Success", {
    expect_silent(assertIsANumberOrNULL(1.1))
    expect_silent(assertIsANumberOrNULL(NULL))
})

test_that("Failure", {
    expect_error(
        assertIsANumberOrNULL(c(1.1, 1.2)),
        "is_a_number : x has length 2, not 1."
    )
})
