context("Ensembl2Entrez")

test_that("Ensembl2Entrez", {
    x <- Ensembl2Entrez(rse)
    expect_s4_class(x, "Ensembl2Entrez")
})
