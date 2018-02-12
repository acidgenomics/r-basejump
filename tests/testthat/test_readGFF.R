context("readGFF")

mousefile <- "http://basejump.seq.cloud/mmusculus.gtf"
mouse <- readGFF(mousefile, quiet = TRUE)

test_that("Mouse", {
    # Check for 9 columns
    expect_identical(
        ncol(mouse),
        9L
    )
})

test_that("Fruitfly", {
    fruitfly <- readGFF("http://basejump.seq.cloud/dmelanogaster.gtf",
                        quiet = TRUE)
    # Check for 9 columns
    expect_identical(
         ncol(fruitfly),
        9L
    )
})

test_that("Invalid files", {
    # Bad URL
    expect_error(
        readGFF("http://basejump.seq.cloud/mtcars.rda", quiet = TRUE),
        "GFF/GTF file failed to load"
    )

    # Bad GFF file
    expect_error(
        readGFF("http://basejump.seq.cloud/mtcars.tsv", quiet = TRUE),
        "GFF/GTF file failed to load"
    )
})

test_that("GTF alias", {
    expect_identical(
        readGTF(mousefile, quiet = TRUE),
        mouse
    )
})
