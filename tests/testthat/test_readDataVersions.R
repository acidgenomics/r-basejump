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

test_that("Missing file", {
    expect_warning(
        readDataVersions("XXX.csv"),
        "XXX.csv missing"
    )
    expect_equal(
        suppressWarnings(
            readDataVersions("XXX.csv")
        ),
        NULL
    )
})
