context("assertFormalColorFunction")

test_that("Success", {
    expect_silent(assertFormalColorFunction(viridis))
})

test_that("Failure", {
    expect_error(assertFormalColorFunction(viridis(256L)))
})
