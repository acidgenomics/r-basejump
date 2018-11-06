context("MGI2Ensembl")

test_that("MGI2Ensembl", {
    x <- MGI2Ensembl()
    expect_s4_class(x, "MGI2Ensembl")
})
