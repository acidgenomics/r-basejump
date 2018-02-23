context("gene2symbolFromGFF")

test_that("Mouse", {
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

test_that("Fruitfly", {
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
    gff <- readGFF(mousefile)
    expect_identical(
        gene2symbolFromGFF(gff),
        mouse
    )
})

test_that("Unique symbol mode", {
    gff <- gene2symbolFromGFF(mousefile, uniqueSymbol = TRUE)
    expect_false(any(duplicated(gff[["symbol"]])))
})

test_that("Invalid number of columns", {
    expect_error(
        gene2symbolFromGFF(mtcars),
        "are_identical : ncol\\(object\\) and 9L are not identical."
    )
})

test_that("GTF alias", {
    expect_identical(
        gene2symbolFromGTF(mousefile),
        mouse
    )
})
