context("readProgramVersions")

test_that("readProgramVersions", {
    versions <- readProgramVersions(
        file.path("http://basejump.seq.cloud",
                  "bcbio",
                  "programs.txt"),
        quiet = TRUE)
    expect_true(tibble::is_tibble(versions))
    expect_equal(
        colnames(versions),
        c("program", "version")
    )
})

test_that("Missing file", {
    expect_warning(
        readProgramVersions("XXX.txt"),
        "XXX.txt missing"
    )
    expect_equal(
        suppressWarnings(
            readProgramVersions("XXX.txt")
        ),
        NULL
    )
})
