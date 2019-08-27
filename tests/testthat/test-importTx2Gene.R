context("importTx2Gene")

test_that("importTx2Gene", {
    object <- importTx2Gene(
        file = file.path("cache", "tx2gene.csv"),
        organism = "Mus musculus",
        genomeBuild = "GRCm38",
        ensemblRelease = 90L
    )
    expect_is(object, "Tx2Gene")
    expect_identical(
        object = colnames(object),
        expected = c("transcriptID", "geneID")
    )
})
