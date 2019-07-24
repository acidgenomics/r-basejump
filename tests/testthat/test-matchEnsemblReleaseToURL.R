context("matchEnsemblReleaseToURL")

skip_if_not(hasInternet())

test_that("Ensembl 96", {
    expect_identical(
        object = matchEnsemblReleaseToURL(96L),
        expected = "http://apr2019.archive.ensembl.org"
    )
})
