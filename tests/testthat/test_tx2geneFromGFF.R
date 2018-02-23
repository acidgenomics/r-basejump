context("tx2geneFromGFF")

test_that("Mouse", {
    mouse <- tx2geneFromGFF("mmusculus.gtf")
    expect_identical(dim(mouse), c(20L, 2L))
    expect_identical(
        head(mouse, 2L),
        data.frame(
            "enstxp" = c(
                "ENSMUST00000070533",
                "ENSMUST00000082908"),
            "ensgene" = c(
                "ENSMUSG00000051951",
                "ENSMUSG00000064842"),
            row.names = c(
                "ENSMUST00000070533",
                "ENSMUST00000082908"),
            stringsAsFactors = FALSE)
    )
    expect_message(
        tx2geneFromGFF("mmusculus.gtf"),
        "tx2gene mappings: 20 transcripts, 17 genes"
    )
})

test_that("Fruitfly", {
    fruitfly <- tx2geneFromGFF("dmelanogaster.gtf")
    expect_identical(dim(fruitfly), c(7L, 2L))
    expect_identical(
        head(fruitfly, 2L),
        data.frame(
            "enstxp" = c(
                "FBtr0070000",
                "FBtr0070001"),
            "ensgene" = c(
                "FBgn0031081",
                "FBgn0052826"),
            row.names = c(
                "FBtr0070000",
                "FBtr0070001"),
            stringsAsFactors = FALSE)
    )
})

test_that("GFF data.frame input", {
    gff <- readGFF("mmusculus.gtf")
    expect_identical(
        tx2geneFromGFF("mmusculus.gtf"),
        tx2geneFromGFF(gff)
    )
})

test_that("Invalid number of columns", {
    expect_error(
        tx2geneFromGFF(mtcars),
        "are_identical : ncol\\(object\\) and 9L are not identical."
    )
})
