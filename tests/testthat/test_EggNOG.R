context("EggNOG")

test_that("EggNOG", {
    x <- EggNOG()
    expect_s4_class(x, "EggNOG")
})
