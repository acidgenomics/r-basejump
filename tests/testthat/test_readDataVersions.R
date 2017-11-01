context("readDataVersions")

test_that("readDataVersions", {
    versions <- readDataVersions(
        file.path("http://basejump.seq.cloud",
                  "bcbio",
                  "data_versions.csv"),
        quiet = TRUE)
    expect_true(tibble::is_tibble(versions))
    expect_equal(
        colnames(versions),
        c("genome", "resource", "version")
    )
})
