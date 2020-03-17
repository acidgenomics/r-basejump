context("matchEnsemblReleaseToURL")

skip_if_not(hasInternet(url = "https://ensembl.org/"))

test_that("Ensembl 98", {
    expect_identical(
        object = matchEnsemblReleaseToURL(98L),
        expected = "http://sep2019.archive.ensembl.org"
    )
})
