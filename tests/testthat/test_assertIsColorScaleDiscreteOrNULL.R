context("assertIsColorScaleDiscreteOrNULL")

test_that("Success", {
    color <- scale_color_viridis(discrete = TRUE)
    expect_silent(assertIsColorScaleDiscreteOrNULL(color))
    expect_silent(assertIsColorScaleDiscreteOrNULL(NULL))
})

test_that("Failure", {
    color <- scale_color_viridis(discrete = FALSE)
    expect_error(
        assertIsColorScaleDiscreteOrNULL(color),
        paste(
            "is2 :",
            "x is not in any of the classes 'ScaleDiscrete', 'NULL'."
        )
    )
})
