context("tx2gene")

test_that("character", {
    organism <- "Homo sapiens"
    expect_identical(
        tx2gene(organism),
        annotable(organism, format = "tx2gene")
    )
})
