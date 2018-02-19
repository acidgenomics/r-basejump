context("assertAllAreNonExisting")

test_that("Success", {
    expect_silent(assertAllAreNonExisting(c("a", "b", "c")))
})

test_that("Failure", {
    a <- 1L
    b <- 2L
    expect_error(assertAllAreNonExisting(c("a", "b", "c")))
})

