context("assertIsImplicitInteger")

test_that("Success", {
    expect_silent(assertIsImplicitInteger(1L))
    expect_silent(assertIsImplicitInteger(1))  # nolint
    expect_silent(assertIsImplicitInteger(1.0))
})

test_that("Failure", {
    # Check tolerance threshold
    expect_error(
        assertIsImplicitInteger(1.000000000000001)
    )
    expect_error(
        assertIsImplicitInteger(1.1)
    )
    expect_error(
        assertIsImplicitInteger(mtcars)
    )
})
