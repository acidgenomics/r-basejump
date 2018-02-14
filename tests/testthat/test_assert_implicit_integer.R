context("assert_is_implicit_integer")

test_that("Success", {
    expect_silent(assert_is_implicit_integer(1L))
    expect_silent(assert_is_implicit_integer(1))  # nolint
    expect_silent(assert_is_implicit_integer(1.0))
})

test_that("Failure", {
    # Check tolerance threshold
    expect_error(
        assert_is_implicit_integer(1.000000000000001)
    )
    expect_error(
        assert_is_implicit_integer(1.1)
    )
    expect_error(
        assert_is_implicit_integer(mtcars)
    )
})
