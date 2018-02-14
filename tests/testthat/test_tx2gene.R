context("tx2gene")

test_that("Character", {
    organism <- "Homo sapiens"
    expect_identical(
        tx2gene(organism),
        annotable(organism, format = "tx2gene")
    )
})
