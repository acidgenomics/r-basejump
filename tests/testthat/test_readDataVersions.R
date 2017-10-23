context("readDataVersions")

test_that("readDataVersions", {
    versions <- readDataVersions(
        "http://basejump.seq.cloud/data_versions.csv",
        quiet = TRUE)
    expect_true(tibble::is_tibble(versions))
    expect_equal(
        colnames(versions),
        c("genome", "resource", "version")
    )
})
