context("toStringUnique")

test_that("toStringUnique", {
    vec <- c("hello", "world", NA, "hello", "world", NA)
    expect_identical(
        toStringUnique(vec),
        "hello, world"
    )
})
