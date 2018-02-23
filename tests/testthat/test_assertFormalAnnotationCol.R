context("assertFormalAnnotationCol")

test_that("Success", {
    expect_silent(assertFormalAnnotationCol(x, colData))
    expect_silent(assertFormalAnnotationCol(x, NA))
    expect_silent(assertFormalAnnotationCol(x, NULL))
})

test_that("Failure", {
    expect_error(assertFormalAnnotationCol(mtcars, colData))
})
