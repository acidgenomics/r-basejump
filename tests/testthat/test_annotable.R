context("annotable")

test_that("Human", {
    human <- annotable("Homo sapiens", release = 88L)
    expect_equal(
        dim(human),
        c(64592L, 10L)
    )
    expect_equal(
        rownames(human)[1L:5L],
        c("ENSG00000000003",
          "ENSG00000000005",
          "ENSG00000000419",
          "ENSG00000000457",
          "ENSG00000000460")
    )
    expect_equal(
        lapply(human, class),
        list(
            ensgene = "character",
            symbol = "character",
            description = "character",
            biotype = "character",
            broadClass = "character",
            geneSeqStart = "integer",
            geneSeqEnd = "integer",
            seqName = "character",
            seqStrand = "integer",
            seqCoordSystem = "character"
        )
    )
})

test_that("Mouse", {
    # Mouse
    mouse <- annotable("Mus musculus", release = 88L)
    expect_equal(
        dim(mouse),
        c(51158L, 10L)
    )
    expect_equal(
        rownames(mouse)[1L:5L],
        c("ENSMUSG00000000001",
          "ENSMUSG00000000003",
          "ENSMUSG00000000028",
          "ENSMUSG00000000031",
          "ENSMUSG00000000037")
    )
})

test_that("Unsupported Ensembl release", {
    expect_warning(
        annotable("Mus musculus", release = 86L),
        "AnnotationHub only supports Ensembl releases 87 and newer.",
        "Using current release instead."
    )
})

test_that("Bad input", {
    expect_error(
        annotable(c("human", "mouse")),
        "Object must be a string"
    )
    expect_error(
        annotable("Homo sapiens", format = "XXX"),
        "Unsupported format"
    )
    expect_error(
        annotable("XXX"),
        "Failed to detect supported organism"
    )
})

test_that("Legacy", {
    # Check for legacy code support using `release = "current"`
    expect_equal(
        annotable("Homo sapiens", release = "current", quiet = TRUE),
        annotable("Homo sapiens", quiet = TRUE)
    )
})
