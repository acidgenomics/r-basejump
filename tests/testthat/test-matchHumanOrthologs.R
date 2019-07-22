context("matchHumanOrthologs")

skip_if_not(hasInternet())

test_that("Mus musculus", {
    genes <- c(
        "ENSMUSG00000000001", "ENSMUSG00000000003",
        "ENSMUSG00000000028", "ENSMUSG00000000031",
        "ENSMUSG00000000037", "ENSMUSG00000000049"
    )
    map <- matchHumanOrthologs(genes, ensemblRelease = 87L)
    expect_s4_class(map, "DataFrame")
    expect_identical(
        object = map,
        expected = DataFrame(
            geneID = genes,
            hgncID = c(
                "ENSG00000065135",
                NA,
                "ENSG00000093009",
                NA,
                "ENSG00000102098",
                "ENSG00000091583"
            ),
            geneName = c(
                "Gnai3",
                "Pbsn",
                "Cdc45",
                "H19",
                "Scml2",
                "Apoh"
            ),
            hgncName = c(
                "GNAI3",
                NA,
                "CDC45",
                NA,
                "SCML2",
                "APOH"
            ),
            row.names = genes
        )
    )
})
