context("assertIsHexColorFunctionOrNULL")

test_that("Success", {
    expect_silent(assertIsHexColorFunctionOrNULL(viridis))
})

test_that("Failure", {
    expect_error(assertIsHexColorFunctionOrNULL(viridis(256L)))
})
