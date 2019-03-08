data(rse, envir = environment())



context("EggNOG")

test_that("EggNOG", {
    x <- EggNOG()
    expect_s4_class(x, "EggNOG")
})



context("Ensembl2Entrez")

data(rse, envir = environment())

test_that("Ensembl2Entrez", {
    x <- Ensembl2Entrez(rse)
    expect_s4_class(x, "Ensembl2Entrez")
})



context("Gene2Symbol")

format <- methodFormals(
    f = "Gene2Symbol",
    signature = "SummarizedExperiment",
    package = "basejump"
) %>%
    .[["format"]] %>%
    eval()

with_parameters_test_that(
    "Gene2Symbol", {
        x <- Gene2Symbol(rse, format = format)
        expect_s4_class(x, "Gene2Symbol")
    },
    format = format
)

rm(format)



context("HGNC2Ensembl")

test_that("HGNC2Ensembl", {
    x <- HGNC2Ensembl()
    expect_s4_class(x, "HGNC2Ensembl")
})




context("MGI2Ensembl")

test_that("MGI2Ensembl", {
    x <- MGI2Ensembl()
    expect_s4_class(x, "MGI2Ensembl")
})



context("PANTHER")

test_that("PANTHER", {
    x <- PANTHER(organism = "Homo sapiens")
    expect_s4_class(x, "PANTHER")
})



context("Tx2Gene")

data(tx_se, envir = environment())

test_that("Tx2Gene", {
    x <- Tx2Gene(tx_se)
    expect_s4_class(x, "Tx2Gene")
})
