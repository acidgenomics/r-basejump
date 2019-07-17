context("lfcThreshold")

test_that("Annotated", {
    expect_null(lfcThreshold(rse))
    lfcThreshold(rse) <- 0.5
    expect_identical(lfcThreshold(rse), 0.5)
})
