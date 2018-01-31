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

test_that("Human GRCh37/hg19 genome build support", {
    # Load up the internal data
    load(system.file(
        file.path("extdata", "grch37.rda"),
        package = "basejump"
    ))
    load(system.file(
        file.path("extdata", "grch37Tx2gene.rda"),
        package = "basejump"
    ))

    expect_identical(
        annotable("Homo sapiens", genomeBuild = "GRCh37"),
        grch37
    )

    # Also support `hg19` as genomeBuild name
    expect_identical(
        annotable("Homo sapiens", genomeBuild = "hg19"),
        grch37
    )

    # gene2symbol
    expect_identical(
        annotable(
            "Homo sapiens",
            genomeBuild = "GRCh37",
            format = "gene2symbol"),
        grch37[, c("ensgene", "symbol")]
    )

    # tx2gene
    expect_identical(
        annotable(
            "Homo sapiens",
            genomeBuild = "GRCh37",
            format = "tx2gene"),
        grch37Tx2gene
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

test_that("annotables package data.frame input", {
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

test_that("Legacy release parameter support", {
    # Check for legacy code support using `release = "current"`
    expect_equal(
        annotable("Homo sapiens", release = "current", quiet = TRUE),
        annotable("Homo sapiens", quiet = TRUE)
    )
})

test_that("NULL input", {
    expect_identical(annotable(NULL), NULL)
})

test_that("Unique symbols", {
    anno <- annotable(
        "Homo sapiens",
        uniqueSymbol = TRUE,
        quiet = TRUE)
    g2s <- annotable(
        "Homo sapiens",
        format = "gene2symbol",
        uniqueSymbol = TRUE,
        quiet = TRUE)
    # Check for `.1` in `symbol` column
    expect_true(any(grepl("\\.1$", anno[["symbol"]])))
    expect_true(any(grepl("\\.1$", g2s[["symbol"]])))
    # Ensure that symbols aren't duplicated
    expect_true(!any(duplicated(anno[["symbol"]])))
    expect_true(!any(duplicated(g2s[["symbol"]])))
})

test_that("Collapse annotables tibble", {
    loadRemoteData(
        file.path(
            "https://github.com",
            "stephenturner",
            "annotables",
            "raw",
            "master",
            "data",
            "grch38.rda"),
        quiet = TRUE)
    expect_true(any(duplicated(grch38[["ensgene"]])))
    annotable <- annotable(grch38)
    expect_true(!any(duplicated(annotable[["ensgene"]])))
})
