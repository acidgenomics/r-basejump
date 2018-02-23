context("assertHasRownames")

test_that("Success", {
    expect_silent(assertHasRownames(data))
})

test_that("Failure", {
    rownames(data) <- NULL
    expect_error(assertHasRownames(data))
    expect_error(assertHasRownames(tibble))
})
