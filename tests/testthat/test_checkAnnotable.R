context("checkAnnotable")

test_that("Success", {
    annotable <- annotable("Homo sapiens", quiet = TRUE)
    expect_true(checkAnnotable(annotable))
})

test_that("Failure", {
    expect_error(
        checkAnnotable(mtcars),
        "Annotable must contain:"
    )
})

test_that("Object isn't a data.frame", {
    expect_error(
        checkAnnotable(NULL),
        "Annotable must be a data.frame"
    )
})
