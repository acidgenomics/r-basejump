context("EggNOG")

skip_if_not(hasInternet())

object <- EggNOG()

test_that("EggNOG", {
    expect_s4_class(object, "EggNOG")
    expect_identical(length(object), 2L)
})

test_that("show", {
    expect_output(
        object = show(object),
        regexp = "EggNOG"
    )
})
