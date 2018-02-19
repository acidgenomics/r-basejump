context("assertIsCharacterOrNULL")

test_that("Success", {
    expect_silent(assertIsCharacterOrNULL(c("hello", "world")))
    expect_silent(assertIsCharacterOrNULL(NULL))
})

test_that("Failure", {
    expect_error(
        assertIsCharacterOrNULL(1L),
        "is2 : x is not in any of the classes 'character', 'NULL'."
    )
})
