context("Tx2Gene")

data(tx_se, envir = environment())

test_that("Tx2Gene", {
    x <- Tx2Gene(tx_se)
    expect_s4_class(x, "Tx2Gene")
})
