context("assertIsFillScaleDiscreteOrNULL")

test_that("Success", {
    fill <- scale_fill_viridis(discrete = TRUE)
    expect_silent(assertIsFillScaleDiscreteOrNULL(fill))
    expect_silent(assertIsFillScaleDiscreteOrNULL(NULL))
})

test_that("Failure", {
    fill <- scale_fill_viridis(discrete = FALSE)
    expect_error(
        assertIsFillScaleDiscreteOrNULL(fill),
        paste(
            "is2 :",
            "x is not in any of the classes 'ScaleDiscrete', 'NULL'."
        )
    )
})
