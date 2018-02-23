context("readGFF")

test_that("Mouse", {
    mouse <- readGFF("mmusculus.gtf")
    # Check for 9 columns
    expect_identical(ncol(mouse), 9L)
})

test_that("Fruitfly", {
    fruitfly <- readGFF("dmelanogaster.gtf")
    # Check for 9 columns
    expect_identical(ncol(fruitfly), 9L)
})

test_that("Invalid files", {
    expect_error(
        readGFF("mtcars.rda"),
        "GFF/GTF file failed to load"
    )
    expect_error(
        readGFF("mtcars.tsv"),
        "GFF/GTF file failed to load"
    )
})
