## FIXME Improve URL failure message.
## FIXME Skip if user doesn't have internet.

context("MGI2Ensembl")

## FIXME Improve URL failure message.
test_that("MGI2Ensembl", {
    skip_if_not(hasInternet())
    object <- MGI2Ensembl()
    expect_s4_class(object, "MGI2Ensembl")
})
