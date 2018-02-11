context("annotable")

test_that("Ensembl versioned release", {
    release <- 88L
    human <- suppressMessages(annotable("Homo sapiens", release = release))
    mouse <- suppressMessages(annotable("Mus musculus", release = release))

    expect_equal(
        dim(human),
        c(64592L, 11L)
    )
    expect_equal(
        dim(mouse),
        c(51158L, 11L)
    )

    expect_equal(
        rownames(human)[1L:3L],
        c("ENSG00000000003", "ENSG00000000005", "ENSG00000000419")
    )
    expect_equal(
        rownames(mouse)[1L:3L],
        c("ENSMUSG00000000001", "ENSMUSG00000000003", "ENSMUSG00000000028")
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
    # Compare against the internally stashed data
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
        paste(
            "is_subset :",
            "The elements 'description', 'biotype' in annotableCols",
            "are not in colnames\\(object\\)."
        )
    )
})

test_that("Unsupported Ensembl release", {
    expect_error(
        annotable("Mus musculus", release = 86L),
        paste(
            "is_greater_than_or_equal_to :",
            "release are not all greater than or equal to 87L."
        )
    )
})

test_that("Unsupported organism", {
    expect_error(
        annotable("XXX"),
        "Full latin organism name XXX is not supported in AnnotationHub"
    )
})

test_that("Bad input", {
    expect_error(
        annotable(c("human", "mouse")),
        "is_a_string : object has length 2, not 1"
    )
    expect_error(
        annotable("Homo sapiens", format = "XXX"),
        paste(
            "is_subset :",
            "The element 'XXX' in format is not in",
            "c\\(\"gene\", \"gene2symbol\", \"tx2gene\"\\)."
        )
    )
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

test_that("Internal `broadClass` column integrity", {
    expect_error(
        .defineBroadClass(mtcars),
        paste(
            "is_subset :",
            "The elements 'biotype', 'symbol' in",
            "c\\(\"biotype\", \"symbol\"\\)",
            "are not in colnames\\(object\\)."
        )
    )
})
