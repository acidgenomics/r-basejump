context("checkAnnotable")

test_that("Success", {
    annotable <- annotable("Homo sapiens", quiet = TRUE)
    expect_true(checkAnnotable(annotable))
})

test_that("Failure", {
    expect_error(
        checkAnnotable(mtcars),
        "annotable must contain:"
    )
})
