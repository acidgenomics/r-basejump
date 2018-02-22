context("annotable")

test_that("Ensembl versioned release", {
    release <- 87L
    human <- annotable("Homo sapiens", release = release)
    mouse <- annotable("Mus musculus", release = release)

    expect_identical(
        dim(human),
        c(63970L, 11L)
    )
    expect_identical(
        dim(mouse),
        c(50143L, 11L)
    )

    expect_identical(
        rownames(human)[1L:3L],
        c("ENSG00000000003", "ENSG00000000005", "ENSG00000000419")
    )
    expect_identical(
        rownames(mouse)[1L:3L],
        c("ENSMUSG00000000001", "ENSMUSG00000000003", "ENSMUSG00000000028")
    )

    expect_identical(
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
    load(system.file("extdata/grch37.rda", package = "basejump"))
    load(system.file("extdata/grch37Tx2gene.rda", package = "basejump"))

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

test_that("Annotables package data frame input", {
    # Use the pre-compiled grch37 annotable from annotables package
    loadRemoteData("http://basejump.seq.cloud/grch37.rda")

    # This is fast but will drop extra columns
    human <- annotable(grch37)
    expect_identical(
        dim(human),
        c(63677L, 10L)
    )
    expect_identical(
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
        "is_subset"
    )
})

test_that("DataFrame coercion AsIs list", {
    loadRemoteData("http://basejump.seq.cloud/annotable_AsIs.rda")
    expect_warning(
        annotable(annotable_AsIs),
        paste(
            "Genes without annotations:",
            "ENSMUSG00000101738, ENSMUSG00000104475, ENSMUSG00000109048"
        )
    )
    # Check to ensure AsIs list is coerced to list
    annotable <- suppressWarnings(annotable(annotable_AsIs))
    expect_is(annotable[["entrez"]], "list")
})

test_that("Collapse annotables tibble", {
    loadRemoteData(
        paste(
            "https://github.com",
            "stephenturner",
            "annotables",
            "raw",
            "master",
            "data",
            "grch38.rda",
            sep = "/"))
    expect_true(any(duplicated(grch38[["ensgene"]])))
    annotable <- annotable(grch38)
    expect_true(!any(duplicated(annotable[["ensgene"]])))
})

test_that("Unique symbols", {
    anno <- annotable("Homo sapiens", uniqueSymbol = TRUE)
    g2s <- annotable(
        object = "Homo sapiens",
        format = "gene2symbol",
        uniqueSymbol = TRUE)
    # Check for `.1` in `symbol` column
    expect_true(any(grepl("\\.1$", anno[["symbol"]])))
    expect_true(any(grepl("\\.1$", g2s[["symbol"]])))
    # Ensure that symbols aren't duplicated
    expect_true(!any(duplicated(anno[["symbol"]])))
    expect_true(!any(duplicated(g2s[["symbol"]])))
})

test_that("Unsupported Ensembl release", {
    expect_error(
        annotable("Mus musculus", release = 86L),
        "is_greater_than_or_equal_to : "
    )
})

test_that("Unsupported organism", {
    expect_error(
        annotable("XXX"),
        "XXX is not supported in AnnotationHub"
    )
})

test_that("Bad input", {
    expect_error(
        annotable(c("human", "mouse")),
        "is_a_string : "
    )
    expect_error(
        annotable("Homo sapiens", format = "XXX"),
        "is_subset : "
    )
})

test_that("Internal `broadClass` column integrity", {
    expect_error(
        .defineBroadClass(mtcars),
        "is_subset : "
    )
})
