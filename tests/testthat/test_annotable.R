context("annotable")

test_that("Human", {
    human <- annotable("Homo sapiens", release = 88L)
    expect_equal(
        dim(human),
        c(64592L, 11L)
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
            seqCoordSystem = "character",
            entrez = "list"
        )
    )
})

test_that("Mouse", {
    # Mouse
    mouse <- annotable("Mus musculus", release = 88L)
    expect_equal(
        dim(mouse),
        c(51158L, 11L)
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

test_that("annotables data.frame", {
    # Use the pre-compiled grch37 annotable from annotables package
    loadRemoteData("http://basejump.seq.cloud/grch37.rda", quiet = TRUE)

    # This is fast but will drop extra columns
    human <- annotable(grch37)
    expect_equal(
        dim(human),
        c(63677L, 10L)
    )
    expect_equal(
        lapply(human, class),
        list(
            ensgene = "character",
            symbol = "character",
            description = "character",
            biotype = "character",
            broadClass = "character",
            chr = "character",
            start = "integer",
            end = "integer",
            strand = "integer",
            entrez = "list"
        )
    )

    malformed <- grch37[, c("ensgene", "symbol")]
    expect_error(
        annotable(malformed),
        "Required columns: ensgene, symbol, description, biotype"
    )
})

test_that("Unsupported Ensembl release", {
    expect_warning(
        annotable("Mus musculus", release = 86L),
        "AnnotationHub only supports Ensembl releases 87 and newer.",
        "Using current release instead."
    )
})

test_that("Unsupported organism", {
    expect_warning(
        annotable("XXX"),
        "XXX is not supported in AnnotationHub"
    )
    expect_equal(
        suppressWarnings(annotable("XXX")),
        NULL
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
})

test_that("Legacy", {
    # Check for legacy code support using `release = "current"`
    expect_equal(
        annotable("Homo sapiens", release = "current", quiet = TRUE),
        annotable("Homo sapiens", quiet = TRUE)
    )
})

test_that("GRCh37/hg19", {
    grch37 <- annotable("Homo sapiens", genomeBuild = "GRCh37")
    hg19 <- annotable("Homo sapiens", genomeBuild = "hg19")
    expect_identical(grch37, hg19)
    expect_identical(grch37, basejump::grch37)

    # gene2symbol
    gene2symbol <- annotable(
        "Homo sapiens",
        genomeBuild = "GRCh37",
        format = "gene2symbol")
    expect_identical(
        basejump::grch37[, c("ensgene", "symbol")],
        gene2symbol
    )

    # tx2gene
    tx2gene <- annotable(
        "Homo sapiens",
        genomeBuild = "GRCh37",
        format = "tx2gene")
    expect_identical(
        basejump::grch37Tx2gene,
        tx2gene
    )
})
