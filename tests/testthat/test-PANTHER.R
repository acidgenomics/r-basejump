context("PANTHER")

object <- PANTHER("Homo sapiens")

test_that("PANTHER", {
    expect_s4_class(object, "PANTHER")
    expect_identical(length(object), 9L)
})

test_that("PANTHER", {
    expect_output(
        object = show(object),
        regexp = "PANTHER"
    )
})
