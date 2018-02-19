context("assertIsAnIntegerOrNULL")

test_that("Success", {
    expect_silent(assertIsAnIntegerOrNULL(1L))
    expect_silent(assertIsAnIntegerOrNULL(NULL))
})

test_that("Failure", {
    expect_error(
        assertIsAnIntegerOrNULL(c(1L, 2L)),
        "is_an_integer : x has length 2, not 1."
    )
})
