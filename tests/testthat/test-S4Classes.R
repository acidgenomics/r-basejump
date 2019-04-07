# Running full checks in `testthat-extra/` directory.
# These can take too long on CI and cause builds to time out otherwise.
test <- getOption("acid.test")
options(acid.test = TRUE)



context("EggNOG")

object <- EggNOG()

test_that("EggNOG", {
    expect_s4_class(object, "EggNOG")
    expect_identical(length(object), 2L)
})

test_that("show", {
    expect_output(
        object = show(object),
        regexp = "EggNOG"
    )
})



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

test_that("summary", {
    x <- Gene2Symbol(rse)
    output <- capture.output(summary(x))
    expect_identical(
        output,
        c(
            "genes: 500",
            "symbols: 500",
            "format: makeUnique",
            "organism: Homo sapiens",
            "genomeBuild: GRCh38",
            "ensemblRelease: 92",
            "id: AH60977",
            "version: 0.1.0",
            "date: 2019-03-27"
        )
    )
})



context("Tx2Gene")

test_that("Tx2Gene", {
    x <- Tx2Gene(txse)
    expect_s4_class(x, "Tx2Gene")
})

test_that("summary", {
    x <- Tx2Gene(txse)
    output <- capture.output(summary(x))
    expect_identical(
        output,
        c(
            "transcripts: 6",
            "genes: 2",
            "organism: Homo sapiens",
            "genomeBuild: GRCh38",
            "ensemblRelease: 92",
            "id: AH60977",
            "version: 0.1.0",
            "date: 2019-03-27"
        )
    )
})



options(acid.test = test)
