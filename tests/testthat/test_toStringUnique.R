context("toStringUnique")

test_that("Atomic", {
    vec <- c("hello", "world", NA, "hello", "world", NA)
    expect_identical(
        toStringUnique(vec),
        "hello, world"
    )
    expect_error(
        toStringUnique(mtcars),
        "is_atomic"
    )
})
