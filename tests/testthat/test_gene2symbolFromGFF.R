context("gene2symbolFromGFF")

mousefile <- "http://basejump.seq.cloud/mmusculus.gtf"
mouse <- gene2symbolFromGFF(mousefile, quiet = TRUE)

test_that("mouse", {
    expect_identical(
        dim(mouse),
        c(17L, 2L)
    )
    expect_identical(
        head(mouse, 2L),
        data.frame(
            "ensgene" = c("ENSMUSG00000025900",
                          "ENSMUSG00000051951"),
            "symbol" = c("Rp1",
                         "Xkr4"),
            row.names = c("ENSMUSG00000025900",
                          "ENSMUSG00000051951"),
            stringsAsFactors = FALSE)
    )
})

test_that("fruitfly", {
    flyfile <- "http://basejump.seq.cloud/dmelanogaster.gtf"
    fruitfly <- gene2symbolFromGFF(flyfile, quiet = TRUE)
    expect_identical(
        dim(fruitfly),
        c(5L, 2L)
    )
    expect_identical(
        head(fruitfly, 2L),
        data.frame(
            ensgene = c("FBgn0031081",
                        "FBgn0031085"),
            symbol = c("Nep3",
                       "CG9570"),
            row.names = c("FBgn0031081",
                          "FBgn0031085"),
            stringsAsFactors = FALSE)
    )
})

test_that("GFF data.frame input", {
    gff <- readGFF(
        mousefile,
        quiet = TRUE)
    expect_identical(
        gene2symbolFromGFF(gff, quiet = TRUE),
        mouse
    )
})

test_that("GTF alias", {
    expect_identical(
        gene2symbolFromGTF(mousefile, quiet = TRUE),
        mouse
    )
})
