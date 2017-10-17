context("readGFF")

test_that("readGFF", {
    # Check for 9 columns
    expect_equal(
        readGFF(
            file.path(testDataURL, "mmusculus.gtf"),
            quiet = TRUE) %>%
            dim() %>%
            .[[2L]],
        9L
    )

    # Check for 9 columns
    expect_equal(
        readGFF(
            file.path(testDataURL, "dmelanogaster.gtf"),
            quiet = TRUE) %>%
            dim() %>%
            .[[2L]],
        9L
    )

    # Bad URL
    expect_error(
        readGFF(
            file.path(testDataURL, "mtcars.rda"),
            quiet = TRUE),
        "GFF file failed to load"
    )

    # Bad GFF file
    expect_error(
        readGFF(
            file.path(testDataURL, "mtcars.tsv"),
            quiet = TRUE),
        "GFF file failed to load"
    )
})
