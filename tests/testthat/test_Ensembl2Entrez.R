context("Ensembl2Entrez")

data(rse, envir = environment())

test_that("Ensembl2Entrez", {
    x <- Ensembl2Entrez(rse)
    expect_s4_class(x, "Ensembl2Entrez")
})
