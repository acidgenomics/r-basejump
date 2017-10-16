context("coerce")

test_that("coerce", {
    expect_true(
        mtcars %>%
            as("tibble") %>%
            tibble::is_tibble(x = .))
    expect_error(
        c("foo", "bar") %>%
            as("tibble"),
        "Object must support 'dim'"
    )
})
