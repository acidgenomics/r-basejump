context("gene2symbolFromGFF")

mouse <- gene2symbolFromGFF(
    file.path(testDataURL, "mmusculus.gtf"),
    quiet = TRUE)

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

test_that("GFF data.frame input", {
    gtf <- readGFF(
        file.path(testDataURL, "mmusculus.gtf"),
        quiet = TRUE)
    expect_equal(
        gene2symbolFromGFF(gtf, quiet = TRUE),
        mouse
    )
})

fruitfly <- gene2symbolFromGFF(
    file.path(testDataURL, "dmelanogaster.gtf"),
    quiet = TRUE)

test_that("fruitfly", {
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
