context("gene2symbolFromGFF")

mousefile <- file.path(testDataURL, "mmusculus.gtf")
mouse <- gene2symbolFromGFF(mousefile, quiet = TRUE)

test_that("mouse", {
    expect_equal(
        dim(mouse),
        c(17L, 2L)
    )
    expect_equal(
        head(mouse, 2L),
        data.frame(
            ensgene = c("ENSMUSG00000025900",
                        "ENSMUSG00000051951"),
            symbol = c("Rp1",
                       "Xkr4"),
            row.names = c("ENSMUSG00000025900",
                          "ENSMUSG00000051951"))
    )
})

test_that("fruitfly", {
    flyfile <- file.path(testDataURL, "dmelanogaster.gtf")
    fruitfly <- gene2symbolFromGFF(flyfile, quiet = TRUE)
    expect_equal(
        dim(fruitfly),
        c(5L, 2L)
    )
    expect_equal(
        head(fruitfly, 2L),
        data.frame(
            ensgene = c("FBgn0031081",
                        "FBgn0031085"),
            symbol = c("Nep3",
                       "CG9570"),
            row.names = c("FBgn0031081",
                          "FBgn0031085"))
    )
})

test_that("GFF data.frame input", {
    gtf <- readGFF(
        mousefile,
        quiet = TRUE)
    expect_equal(
        gene2symbolFromGFF(gtf, quiet = TRUE),
        mouse
    )
})

test_that("GTF alias", {
    expect_equal(
        gene2symbolFromGTF(mousefile, quiet = TRUE),
        mouse
    )
})
