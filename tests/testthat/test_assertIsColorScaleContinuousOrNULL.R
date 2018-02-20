context("assertIsColorScaleContinuousOrNULL")

test_that("Success", {
    color <- scale_color_viridis(discrete = FALSE)
    expect_silent(assertIsColorScaleContinuousOrNULL(color))
    expect_silent(assertIsColorScaleContinuousOrNULL(NULL))
})

test_that("Failure", {
    color <- scale_color_viridis(discrete = TRUE)
    expect_error(
        assertIsColorScaleContinuousOrNULL(color),
        paste(
            "is2 :",
            "x is not in any of the classes 'ScaleContinuous', 'NULL'."
        )
    )
})
