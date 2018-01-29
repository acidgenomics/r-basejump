contest("sanitizeAnnotable")

test_that("Human", {
    human <- annotable("Homo sapiens", quiet = TRUE)
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
