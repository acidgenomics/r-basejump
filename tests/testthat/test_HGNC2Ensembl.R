context("HGNC2Ensembl")

test_that("HGNC2Ensembl", {
    x <- HGNC2Ensembl()
    expect_s4_class(x, "HGNC2Ensembl")
})
