context("matchEnsemblReleaseToURL")

test_that("Ensembl 96", {
    expect_identical(
        object = matchEnsemblReleaseToURL(96L),
        expected = "http://apr2019.archive.ensembl.org"
    )
})
