## FIXME Improve URL failure message.
## FIXME Skip if user doesn't have internet.

context("HGNC2Ensembl")

test_that("HGNC2Ensembl", {
    object <- HGNC2Ensembl()
    expect_s4_class(object, "HGNC2Ensembl")
})
