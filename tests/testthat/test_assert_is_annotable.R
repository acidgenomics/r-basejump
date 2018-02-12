context("assert_is_annotable")

test_that("Success", {
    annotable <- annotable("Homo sapiens", quiet = TRUE)
    expect_silent(assert_is_annotable(annotable))
})

test_that("Failure", {
    expect_error(
        assert_is_annotable(mtcars),
        "is_subset"
    )
})

test_that("Object isn't a data.frame", {
    expect_error(
        assert_is_annotable(NULL),
        "is_data.frame"
    )
})
