context("assertIsFillScaleContinuousOrNULL")

test_that("Success", {
    fill <- scale_fill_viridis(discrete = FALSE)
    expect_silent(assertIsFillScaleContinuousOrNULL(fill))
    expect_silent(assertIsFillScaleContinuousOrNULL(NULL))
})

test_that("Failure", {
    fill <- scale_fill_viridis(discrete = TRUE)
    expect_error(
        assertIsFillScaleContinuousOrNULL(fill),
        paste(
            "is2 :",
            "x is not in any of the classes 'ScaleContinuous', 'NULL'."
        )
    )
})
