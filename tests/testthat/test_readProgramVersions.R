context("readProgramVersions")

test_that("readProgramVersions", {
    versions <- readProgramVersions(
        "http://basejump.seq.cloud/programs.txt",
        quiet = TRUE)
    expect_true(tibble::is_tibble(versions))
    expect_equal(
        colnames(versions),
        c("program", "version")
    )
})
