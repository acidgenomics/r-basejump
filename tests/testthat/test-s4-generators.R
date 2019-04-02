context("Ensembl2Entrez")

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



context("Tx2Gene")

test_that("Tx2Gene", {
    x <- Tx2Gene(txse)
    expect_s4_class(x, "Tx2Gene")
})
