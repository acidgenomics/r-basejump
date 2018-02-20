context("assertIsAHeaderLevel")

test_that("Success", {
    expect_silent(assertIsAHeaderLevel(1L))
})

test_that("Failure", {
    expect_error(
        assertIsAHeaderLevel(8L),
        paste(
            "is_subset :",
            "The element '8' in as.integer\\(x\\) is not in seq\\(1L:7L\\)."
        )
    )
})
