data(mat, rse, package = "acidtest", envir = environment())



context("Transcript sanitization")

test_that("stripTranscriptVersions : character", {
    # Return unmodified if not Ensembl transcript (ENS*T).
    # For example, check and make sure *C. elegans* transcripts are preserved.
    expect_identical(
        object = stripTranscriptVersions(c(
            "ENSMUST00000119854.7",
            "EGFP.1",
            "2L52.1a",
            "2L52.2"
        )),
        expected = c(
            "ENSMUST00000119854",
            "EGFP.1",
            "2L52.1a",
            "2L52.2"
        )
    )
})

test_that("stripTranscriptVersions : matrix", {
    object <- mat
    rownames(object) <- c(
        "ENSMUST00000000001.1",
        "ENSMUST00000000001.2",
        "ENSMUST00000000002.1",
        "EGFP.1"
    )
    expect_identical(
        object = object %>%
            stripTranscriptVersions() %>%
            rownames(),
        expected = c(
            "ENSMUST00000000001",
            "ENSMUST00000000001",  # Dupes allowed in matrix.
            "ENSMUST00000000002",
            "EGFP.1"
        )
    )
})



context("Organism matching")

with_parameters_test_that(
    "organism", {
        expect_identical(
            object = organism(object),
            expected = "Homo sapiens"
        )
    },
    object = list(
        matrix = mat,
        SummarizedExperiment = rse
    )
)



context("Identifier remapping")

# These steps are slow. Consider using cached objects instead.
organism <- "Homo sapiens"
release <- 87L
gene2symbol <-
    makeGene2SymbolFromEnsembl(organism = organism, release = release)
tx2gene <-
    makeTx2GeneFromEnsembl(organism = organism, release = release)

test_that("convertGenesToSymbols : character", {
    expect_identical(
        object = convertGenesToSymbols(
            object = c("ENSG00000000003", "ENSG00000000005"),
            gene2symbol = gene2symbol
        ),
        expected = c(
            ENSG00000000003 = "TSPAN6",
            ENSG00000000005 = "TNMD"
        )
    )
})

test_that("convertGenesToSymbols : matrix", {
    object <- matrix(
        data = seq(1L:4L),
        byrow = TRUE,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(
            c("ENSG00000000003", "ENSG00000000005"),
            c("sample1", "sample2")
        )
    )
    expect_identical(
        object = object %>%
            convertGenesToSymbols(gene2symbol = gene2symbol) %>%
            rownames(),
        expected = c("TSPAN6", "TNMD")
    )
})

# Specify organism (to handle FASTA spike-ins (e.g. EGFP).
test_that("convertGenesToSymbols : FASTA spike-in support", {
    expect_identical(
        object = suppressWarnings(
            convertGenesToSymbols(
                object = c("EGFP", "ENSG00000000003"),
                gene2symbol = gene2symbol
            )
        ),
        expected = c(
            EGFP = "EGFP",
            ENSG00000000003 = "TSPAN6"
        )
    )
})

test_that("convertGenesToSymbols : Invalid identifiers", {
    expect_warning(
        object = convertGenesToSymbols(
            object = "ENSG00000000000",
            gene2symbol = gene2symbol
        ),
        regexp = "Failed to match genes: ENSG00000000000"
    )
    expect_error(
        object = convertGenesToSymbols(c("ENSG00000000003", NA)),
        regexp = "isCharacter"
    )
    expect_error(
        object = convertGenesToSymbols(c("ENSG00000000003", "")),
        regexp = "isCharacter"
    )
})

test_that("convertTranscriptsToGenes : character", {
    expect_identical(
        object = convertTranscriptsToGenes(
            object = c("ENST00000000233", "ENST00000000412"),
            tx2gene = tx2gene
        ),
        expected = factor(c(
            ENST00000000233 = "ENSG00000004059",
            ENST00000000412 = "ENSG00000003056"
        ))
    )
})

test_that("convertTranscriptsToGenes : matrix", {
    object <- matrix(
        data = seq(1L:8L),
        byrow = TRUE,
        nrow = 4L,
        ncol = 2L,
        dimnames = list(
            c(
                "ENST00000373020",
                "ENST00000494424",
                "ENST00000373031",
                "ENST00000485971"
            ),
            c("sample1", "sample2")
        )
    )
    expected <- matrix(
        data = c(4L, 6L, 12L, 14L),
        byrow = TRUE,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(
            c("ENSG00000000003", "ENSG00000000005"),
            c("sample1", "sample2")
        )
    )
    expect_identical(
        object = convertTranscriptsToGenes(
            object = object,
            tx2gene = tx2gene,
            aggregate = TRUE
        ),
        expected = expected
    )
})

test_that("convertTranscriptsToGenes : Invalid params", {
    expect_error(
        object = convertTranscriptsToGenes(
            object = "ENST00000000000",
            tx2gene = tx2gene
        ),
        regexp = "Failed to match transcripts: ENST00000000000"
    )
    expect_error(
        object = convertTranscriptsToGenes(c("ENST00000000233", NA)),
        regexp = "isCharacter"
    )
    expect_error(
        object = convertTranscriptsToGenes(c("ENST00000000233", "")),
        regexp = "isCharacter"
    )
})



context("Annotation databases")

test_that("EggNOG", {
    object <- EggNOG()
    expect_s4_class(object, "EggNOG")
})

test_that("HGNC2Ensembl", {
    object <- HGNC2Ensembl()
    expect_s4_class(object, "HGNC2Ensembl")
})

test_that("MGI2Ensembl", {
    object <- MGI2Ensembl()
    expect_s4_class(object, "MGI2Ensembl")
})

with_parameters_test_that(
    "PANTHER", {
        invisible(capture.output(
            object <- PANTHER(organism)
        ))
        expect_s4_class(object, "PANTHER")
    },
    organism = names(.pantherMappings)
)

# Full organism support is covered in extra checks.
test_that("geneSynonyms", {
    expect_is(
        object = geneSynonyms(organism = "Homo sapiens"),
        class = "grouped_df"
    )
})
