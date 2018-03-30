context("Ensembl Functions")

# convertGenesToSymbols ========================================================
test_that("convertGenesToSymbols : character", {
    x <- convertGenesToSymbols(
        c("ENSMUSG00000000001", "ENSMUSG00000000003"),
        release = ensemblRelease
    )
    expect_identical(
        x,
        c(
            "ENSMUSG00000000001" = "Gnai3",
            "ENSMUSG00000000003" = "Pbsn"
        )
    )
})

test_that("convertGenesToSymbols : matrix", {
    mat <- matrix(
        data = seq(1L:4L),
        byrow = TRUE,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(
            c("ENSMUSG00000000001", "ENSMUSG00000000003"),
            c("sample_1", "sample_2")
        )
    )
    expect_identical(
        convertGenesToSymbols(mat, release = ensemblRelease) %>%
            rownames(),
        c(
            "ENSMUSG00000000001" = "Gnai3",
            "ENSMUSG00000000003" = "Pbsn"
        )
    )
})

test_that("convertGenesToSymbols : FASTA spike-in support", {
    # Specify organism (to handle FASTA spike-ins (e.g. EGFP)
    vec <- c("EGFP", "ENSMUSG00000000001")
    g2s <- suppressWarnings(
        convertGenesToSymbols(
            object = vec,
            organism = "Mus musculus",
            release = ensemblRelease
        )
    )
    expect_identical(g2s, c("EGFP" = "EGFP", "ENSMUSG00000000001" = "Gnai3"))
    expect_warning(
        convertGenesToSymbols(
            object = vec,
            organism = "Mus musculus",
            release = ensemblRelease
        ),
        "Failed to match all genes to symbols: EGFP"
    )
})

test_that("convertGenesToSymbols : Invalid identifiers", {
    expect_warning(
        convertGenesToSymbols("ENSMUSG00000000000", release = ensemblRelease),
        "Failed to match all genes to symbols: ENSMUSG00000000000"
    )
    expect_error(
        convertGenesToSymbols(c("ENSMUSG00000000001", NA)),
        "is_non_missing_nor_empty_character :"
    )
    expect_error(
        convertGenesToSymbols(c("ENSMUSG00000000001", "")),
        "is_non_missing_nor_empty_character :"
    )
})



# convertTranscriptsToGenes ====================================================
test_that("convertTranscriptsToGenes : character", {
    expect_identical(
        convertTranscriptsToGenes(
            c("ENSMUST00000000001", "ENSMUST00000000003"),
            release = ensemblRelease
        ),
        c(
            "ENSMUST00000000001" = "ENSMUSG00000000001",
            "ENSMUST00000000003" = "ENSMUSG00000000003"
        )
    )
    expect_error(
        convertTranscriptsToGenes(
            c("ENSMUST00000000000", "ENSMUST00000000001"),
            release = ensemblRelease
        ),
        "Unmatched transcripts present. Try using a GFF file instead."
    )
    expect_error(
        convertTranscriptsToGenes(c("ENSMUSG00000000001", NA)),
        "is_non_missing_nor_empty_character :"
    )
    expect_error(
        convertTranscriptsToGenes(c("ENSMUSG00000000001", "")),
        "is_non_missing_nor_empty_character :"
    )
})

test_that("convertTranscriptsToGenes : matrix", {
    mat <- matrix(
        data = seq(1L:8L),
        byrow = TRUE,
        nrow = 4L,
        ncol = 2L,
        dimnames = list(
            c(
                "ENSMUST00000000000",
                "ENSMUST00000000001",
                "ENSMUST00000000003",
                "ENSMUST00000114041"
            ),
            c("sample_1", "sample_2")
        )
    )
    expect_error(
        convertTranscriptsToGenes(mat, release = ensemblRelease),
        "Unmatched transcripts present. Try using a GFF file instead."
    )
    expect_identical(
        mat[2L:4L, ] %>%
            convertTranscriptsToGenes() %>%
            rownames(),
        c(
            "ENSMUST00000000001" = "ENSMUSG00000000001",
            "ENSMUST00000000003" = "ENSMUSG00000000003",
            "ENSMUST00000114041" = "ENSMUSG00000000003"
        )
    )
})



# makeGRangesFromEnsembl =======================================================
test_that("makeGRangesFromEnsembl : genes", {
    x <- makeGRangesFromEnsembl(
        organism = "Homo sapiens",
        format = "genes",
        release = ensemblRelease
    )
    expect_s4_class(x, "GRanges")
    expect_identical(length(x), 63970L)
    expect_identical(
        head(names(x), 3L),
        c("ENSG00000000003", "ENSG00000000005", "ENSG00000000419")
    )
    expect_identical(
        lapply(mcols(x), class),
        list(
            "geneID" = "character",
            "geneName" = "character",
            "geneBiotype" = "factor",
            "description" = "character",
            "seqCoordSystem" = "factor",
            "entrezID" = "list",
            "broadClass" = "factor"
        )
    )
})

test_that("makeGRangesFromEnsembl : transcripts", {
    x <- makeGRangesFromEnsembl(
        organism = "Homo sapiens",
        format = "transcripts",
        release = ensemblRelease
    )
    expect_s4_class(x, "GRanges")
    expect_identical(length(x), 216741L)
    expect_identical(
        head(names(x), 3L),
        c("ENST00000000233", "ENST00000000412", "ENST00000000442")
    )
    expect_identical(
        lapply(mcols(x), class),
        list(
            "txID" = "character",
            "txName" = "character",
            "txBiotype" = "factor",
            "geneID" = "character",
            "geneName" = "character",
            "geneBiotype" = "factor",
            "description" = "character",
            "txCdsSeqStart" = "integer",
            "txCdsSeqEnd" = "integer",
            "txSupportLevel" = "factor",
            "seqCoordSystem" = "factor",
            "entrezID" = "AsIs",
            "broadClass" = "factor"
        )
    )
})

test_that("makeGRangesFromEnsembl : GRCh37", {
    # genes
    x <- makeGRangesFromEnsembl(
        organism = "Homo sapiens",
        format = "genes",
        genomeBuild = "GRCh37"
    )
    expect_is(x, "GRanges")
    expect_identical(length(x), 64102L)
    expect_identical(head(names(x), 1L), "ENSG00000000003")

    # transcripts
    x <- makeGRangesFromEnsembl(
        organism = "Homo sapiens",
        format = "transcripts",
        genomeBuild = "GRCh37"
    )
    expect_is(x, "GRanges")
    expect_identical(length(x), 215647L)
    expect_identical(head(names(x), 1L), "ENST00000000233")
})

test_that("makeGRangesFromEnsembl : Unsupported release version", {
    expect_warning(
        makeGRangesFromEnsembl("Homo sapiens", release = 86L),
        "Switching to current release instead."
    )
})

test_that("makeGRangesFromEnsembl : Unsupported organism", {
    expect_error(
        makeGRangesFromEnsembl(organism = "AAA", genomeBuild = "BBB"),
        "No ID matched on AnnotationHub"
    )
})

test_that("makeGRangesFromEnsembl : Multiple organisms", {
    expect_error(
        makeGRangesFromEnsembl(c("Homo sapiens", "Mus musculus")),
        "is_a_string : "
    )
    expect_error(
        makeGRangesFromEnsembl("Homo sapiens", format = "XXX"),
        "'arg' should be one of \"genes\", \"transcripts\""
    )
})



# makeGene2symbolFromEnsembl ===================================================
test_that("makeGene2symbolFromEnsembl", {
    x <- makeGene2symbolFromEnsembl(
        organism = "Homo sapiens",
        release = ensemblRelease
    )
    expect_identical(colnames(x), c("geneID", "geneName"))
    expect_identical(nrow(x), 64661L)
})



# makeTx2geneFromEnsembl =======================================================
test_that("makeTx2geneFromEnsembl", {
    x <- makeTx2geneFromEnsembl(
        organism = "Homo sapiens",
        release = ensemblRelease
    )
    expect_identical(colnames(x), c("txID", "geneID"))
    expect_identical(nrow(x), 220144L)
})
