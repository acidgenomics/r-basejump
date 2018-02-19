context("assertIsDataFrameOrNULL")

test_that("Success", {
    expect_silent(assertIsDataFrameOrNULL(mtcars))
    expect_silent(assertIsDataFrameOrNULL(NULL))
})

test_that("Failure", {
    expect_error(
        assertIsDataFrameOrNULL(1L),
        "is2 : x is not in any of the classes 'data.frame', 'NULL'."
    )
})
