context("tx2geneFromGFF")

mousefile <- file.path(testDataURL, "mmusculus.gtf")
mouse <- tx2geneFromGFF(mousefile, quiet = TRUE)

test_that("mouse", {
    expect_equal(
        dim(mouse),
        c(20L, 2L)
    )
    expect_equal(
        head(mouse, 2L),
        data.frame(
            enstxp = c("ENSMUST00000070533",
                       "ENSMUST00000082908"),
            ensgene = c("ENSMUSG00000051951",
                        "ENSMUSG00000064842"),
            row.names = c("ENSMUST00000070533",
                          "ENSMUST00000082908"))
    )
})

test_that("fruitfly", {
    fruitfly <- tx2geneFromGFF(
        file.path(testDataURL, "dmelanogaster.gtf"),
        quiet = TRUE)
    expect_equal(
        dim(fruitfly),
        c(7L, 2L)
    )
    expect_equal(
        head(fruitfly, 2L),
        data.frame(
            enstxp = c("FBtr0070000",
                       "FBtr0070001"),
            ensgene = c("FBgn0031081",
                        "FBgn0052826"),
            row.names = c("FBtr0070000",
                          "FBtr0070001"))
    )
})

test_that("GFF data.frame input", {
    # Test GFF data.frame input
    gff <- readGFF(mousefile, quiet = TRUE)
    expect_equal(
        tx2geneFromGFF(gff, quiet = TRUE),
        mouse
    )
})

test_that("bad data.frame", {
    expect_error(
        tx2geneFromGFF(mtcars),
        "GFF object must be data.frame with 9 columns"
    )
})

test_that("GTF alias", {
    expect_equal(
        tx2geneFromGTF(mousefile, quiet = TRUE),
        mouse
    )
})
