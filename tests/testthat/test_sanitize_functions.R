context("Sanitize Functions")

# sanitizeAnnotable ============================================================
test_that("sanitizeAnnotable", {
    human <- annotable("Homo sapiens")
    sanitized <- sanitizeAnnotable(human)
    expect_identical(
        lapply(sanitized, class),
        list(
            "ensgene" = "character",
            "symbol" = "character",
            "description" = "character",
            "biotype" = "character",
            "broadClass" = "character",
            "geneSeqStart" = "integer",
            "geneSeqEnd" = "integer",
            "seqName" = "character",
            "seqStrand" = "integer",
            "seqCoordSystem" = "character"
        )
    )
})



# sanitizeColData ==============================================================
test_that("sanitizeColData", {
    x <- coldata
    x[["day"]] <- c(14L, 14L, 30L, 30L)
    x <- sanitizeColData(x)
    expect_is(x, "DataFrame")
    expect_identical(rownames(x), rownames(coldata))
    expect_true(all(vapply(
        X = x,
        FUN = is.factor,
        FUN.VALUE = logical(1L))))
})
