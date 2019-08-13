context("readTx2Gene")

test_that("readTx2Gene", {
    object <- readTx2Gene(
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
