context("alphaThreshold")

test_that("Annotated", {
    expect_null(alphaThreshold(rse))
    alphaThreshold(rse) <- 0.05
    expect_identical(alphaThreshold(rse), 0.05)
})
