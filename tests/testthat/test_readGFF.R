context("readGFF")

mousefile <- "http://basejump.seq.cloud/mmusculus.gtf"
mouse <- readGFF(mousefile, quiet = TRUE)

test_that("mouse", {
    # Check for 9 columns
    expect_equal(
        ncol(mouse),
        9L
    )
})

test_that("fruitfly", {
    fruitfly <- readGFF("http://basejump.seq.cloud/dmelanogaster.gtf",
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
        readGFF("http://basejump.seq.cloud/mtcars.rda", quiet = TRUE),
        "GFF file failed to load"
    )

    # Bad GFF file
    expect_error(
        readGFF("http://basejump.seq.cloud/mtcars.tsv", quiet = TRUE),
        "GFF file failed to load"
    )
})

test_that("GTF alias", {
    expect_equal(
        readGTF(mousefile, quiet = TRUE),
        mouse
    )
})
