context("sem")

test_that("AsIs", {
    x <- I(seq(from = 1L, to = 10L, by = 1L))
    x <- sem(x)
    expect_is(x, "numeric")
})
