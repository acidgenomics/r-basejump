context("importTx2Gene")

args <- list(
    file = file.path("cache", "tx2gene.csv"),
    organism = "Homo sapiens",
    genomeBuild = "GRCh38",
    ensemblRelease = 100L
)

test_that("No version stripping", {
    object <- do.call(
        what = importTx2Gene,
        args = c(
            args,
            ignoreTxVersion = FALSE,
            ignoreGeneVersion = FALSE
        )
    )
    expect_is(object, "Tx2Gene")
    expect_identical(
        object = colnames(object),
        expected = c("transcriptID", "geneID")
    )
})




test_that("importTx2Gene", {
    object <- do.call(
        what = importTx2Gene,
        args = c(
            args,
            ignoreTxVersion = TRUE,
            ignoreGeneVersion = TRUE
        )
    )
    expect_is(object, "Tx2Gene")
    expect_identical(
        object = colnames(object),
        expected = c("transcriptID", "geneID")
    )
})
