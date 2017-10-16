context("readGFF")

test_that("readGFF", {
    mouse <- file.path(testDataURL, "mmusculus.gtf")
    # Check for 9 columns
    expect_equal(
        readGFF(mouse) %>%
            dim %>%
            .[[2L]],
        9L
    )

    fruitfly <- file.path(testDataURL, "dmelanogaster.gtf")
    # Check for 9 columns
    expect_equal(
        readGFF(fruitfly) %>%
            dim %>%
            .[[2L]],
        9L
    )

    # Bad URL
    expect_error(
        readGFF(file.path(testDataURL, "mtcars.rda")),
        "GFF file failed to load"
    )

    # Bad GFF file
    expect_error(
        readGFF(file.path(testDataURL, "mtcars.tsv")),
        "GFF file failed to load"
    )
})
