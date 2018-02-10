context("checkAnnotable")

test_that("Success", {
    annotable <- annotable("Homo sapiens", quiet = TRUE)
    expect_true(checkAnnotable(annotable))
})

test_that("Failure", {
    expect_error(
        checkAnnotable(mtcars),
        "`mtcars` must contain:"
    )
})

test_that("Object isn't a data.frame", {
    expect_error(
        checkAnnotable(NULL),
        "object is not a data frame"
    )
})
