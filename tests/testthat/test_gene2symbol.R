context("gene2symbol")

test_that("Character", {
    organism <- "Homo sapiens"
    expect_identical(
        gene2symbol(organism),
        annotable(organism, format = "gene2symbol")
    )
})
