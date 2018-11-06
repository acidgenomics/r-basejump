context("PANTHER")

test_that("PANTHER", {
    x <- PANTHER(organism = "Homo sapiens")
    expect_s4_class(x, "PANTHER")
})
