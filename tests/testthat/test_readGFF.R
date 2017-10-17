context("readGFF")

mousefile <- file.path(testDataURL, "mmusculus.gtf")
mouse <- readGFF(mousefile, quiet = TRUE)

test_that("mouse", {
    # Check for 9 columns
    expect_equal(
        ncol(mouse),
        9L
    )
})

test_that("fruitfly", {
    fruitfly <- readGFF(
        file.path(testDataURL, "dmelanogaster.gtf"),
        quiet = TRUE)
    # Check for 9 columns
    expect_equal(
         ncol(fruitfly),
        9L
    )
})

test_that("invalid files", {
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

test_that("GTF alias", {
    expect_equal(
        readGTF(mousefile, quiet = TRUE),
        mouse
    )
})
