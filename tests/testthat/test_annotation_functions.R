# TODO Work on stashing gene2symbol, tx2gene here for better speed.



context("Annotation Functions")

release <- 87L



# annotable ====================================================================
test_that("annotable", {
    object <- annotable("Homo sapiens", release = release)
    expect_is(object, "tbl_df")
    expect_true("rowname" %in% colnames(object))
    expect_identical(dim(object), c(63970L, 13L))
    expect_identical(object[["rowname"]][[1L]], "ENSG00000000003")
})



# broadClass ===================================================================
with_parameters_test_that(
    "broadClass", {
        expect_is(broadClass(object), "factor")
    },
    object = list(
        GRanges = makeGRangesFromEnsembl("Homo sapiens"),
        SingleCellExperiment = sce_small
    )
)



# convertGenesToSymbols ========================================================
test_that("convertGenesToSymbols : character", {
    object <- c("ENSMUSG00000000001", "ENSMUSG00000000003")
    expected <- c(
        ENSMUSG00000000001 = "Gnai3",
        ENSMUSG00000000003 = "Pbsn"
    )

    # Using automatic organism detection (AnnotationHub).
    expect_identical(
        convertGenesToSymbols(object, release = release),
        expected = expected
    )

    # Using organism (AnnotationHub).
    expect_identical(
        object = convertGenesToSymbols(
            object = object,
            organism = "Mus musculus",
            release = release
        ),
        expected = expected
    )

    # Using gene2symbol DataFrame.
    g2s <- makeGene2symbolFromEnsembl(
        organism = "Mus musculus",
        release = release
    )
    expect_identical(
        object = convertGenesToSymbols(
            object = object,
            gene2symbol = g2s
        ),
        expected = expected
    )
})

test_that("convertGenesToSymbols : matrix", {
    object <- matrix(
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
        object = object %>%
            convertGenesToSymbols(release = release) %>%
            rownames(),
        expected = c("Gnai3", "Pbsn")
    )
})

test_that("convertGenesToSymbols : FASTA spike-in support", {
    # Specify organism (to handle FASTA spike-ins (e.g. EGFP).
    object <- c("EGFP", "ENSMUSG00000000001")
    expect_identical(
        object = suppressWarnings(
            convertGenesToSymbols(
                object = object,
                organism = "Mus musculus",
                release = release
            )
        ),
        expected = c(EGFP = "EGFP", "ENSMUSG00000000001" = "Gnai3")
    )
})

test_that("convertGenesToSymbols : Invalid identifiers", {
    expect_warning(
        convertGenesToSymbols("ENSMUSG00000000000", release = release),
        "Failed to match genes: ENSMUSG00000000000"
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
    object <- c("ENSMUST00000000001", "ENSMUST00000000003")
    expected <- factor(c(
        "ENSMUST00000000001" = "ENSMUSG00000000001",
        "ENSMUST00000000003" = "ENSMUSG00000000003"
    ))

    # tx2gene (recommended)
    tx2gene <- makeTx2geneFromEnsembl(
        organism = "Mus musculus",
        release = release
    )
    expect_identical(
        object = convertTranscriptsToGenes(object, tx2gene = tx2gene),
        expected = expected
    )

    # organism
    expect_identical(
        object = convertTranscriptsToGenes(
            object = object,
            organism = "Mus musculus",
            release = release
        ),
        expected = expected
    )

    # No tx2gene or organism.
    expect_identical(
        object = convertTranscriptsToGenes(object, release = release),
        expected = expected
    )
})

# TODO Improve the collapse unit test here.
test_that("convertTranscriptsToGenes : matrix", {
    object <- matrix(
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
        object = convertTranscriptsToGenes(object, release = release),
        regexp = "Failed to match transcripts: ENSMUST00000000000"
    )
    expect_identical(
        object = object %>%
            .[2L:4L, ] %>%
            convertTranscriptsToGenes() %>%
            rownames(),
        expected = c("ENSMUSG00000000001", "ENSMUSG00000000003")
    )
})

test_that("convertTranscriptsToGenes : Invalid params", {
    expect_error(
        object = convertTranscriptsToGenes(
            c("ENSMUST00000000000", "ENSMUST00000000001"),
            release = release
        ),
        regexp = "Failed to match transcripts: ENSMUST00000000000"
    )
    expect_error(
        object = convertTranscriptsToGenes(c("ENSMUSG00000000001", NA)),
        regexp = "is_non_missing_nor_empty_character :"
    )
    expect_error(
        object = convertTranscriptsToGenes(c("ENSMUSG00000000001", "")),
        regexp = "is_non_missing_nor_empty_character :"
    )
})



# convertUCSCBuildToEnsembl ====================================================
test_that("convertUCSCBuildToEnsembl", {
    expect_identical(
        object = convertUCSCBuildToEnsembl("hg19"),
        expected = c(hg19 = "GRCh37")
    )
    expect_identical(
        object = convertUCSCBuildToEnsembl("hg38"),
        expected = c(hg38 = "GRCh38")
    )
    expect_identical(
        object = convertUCSCBuildToEnsembl("mm10"),
        expected = c(mm10 = "GRCm38")
    )
    expect_error(
        object = convertUCSCBuildToEnsembl("XXX"),
        regexp = "Failed to match UCSC"
    )
})



# eggnog =======================================================================
test_that("eggnog", {
    object <- eggnog()
    expect_is(object, "list")
    expect_identical(
        object = names(object),
        expected = c("cogFunctionalCategories", "annotations")
    )
    expect_identical(
        object = lapply(object, colnames),
        expected = list(
            cogFunctionalCategories = c(
                "letter",
                "description"
            ),
            annotations = c(
                "eggnogID",
                "consensusFunctionalDescription",
                "cogFunctionalCategory"
            )
        )
    )
})



# emptyRanges ==================================================================
test_that("emptyRanges", {
    object <- emptyRanges("XXX")
    expect_identical(
        object = levels(seqnames(object)),
        expected = "unknown"
    )
    expect_identical(
        object = IRanges::ranges(object),
        expected = IRanges::IRanges(
            start = 1L,
            end = 100L,
            names = "XXX"
        )
    )
})

test_that("emptyRanges : mcols", {
    object <- emptyRanges(
        "EGFP",
        seqname = "transgene",
        mcolsNames = c("geneID", "geneName")
    )
    expect_identical(
        object = names(mcols(object)),
        expected = c("geneID", "geneName")
    )
})



# geneSynonyms =================================================================
with_parameters_test_that(
    "geneSynonyms", {
        object <- geneSynonyms(organism = organism)
        expect_is(object, "grouped_df")
    },
    organism = .geneSynonymsOrganisms
)



# makeGene2symbolFromEnsembl ===================================================
test_that("makeGene2symbolFromEnsembl", {
    object <- makeGene2symbolFromEnsembl(
        organism = "Homo sapiens",
        release = release
    )
    expect_is(object, "gene2symbol")
    expect_identical(nrow(object), 63970L)
})



# makeGene2symbolFromGFF =======================================================
with_parameters_test_that(
    "makeGene2symbolFromGFF", {
        object <- makeGene2symbolFromGFF(file)
        expect_is(object, "gene2symbol")
        expect_identical(
            object = head(object, n = 2L),
            expected = new(
                "gene2symbol",
                DataFrame(
                    geneID = c(
                        "ENSMUSG00000102693",
                        "ENSMUSG00000064842"
                    ),
                    geneName = c(
                        "4933401J01Rik",
                        "Gm26206"
                    ),
                    row.names = c(
                        "ENSMUSG00000102693",
                        "ENSMUSG00000064842"
                    )
                )
            )
        )
    },
    file = c("example.gtf", "example.gff3")
)



# makeGRangesFromEnsembl =======================================================
test_that("makeGRangesFromEnsembl : genes", {
    object <- makeGRangesFromEnsembl(
        organism = "Homo sapiens",
        level = "genes",
        release = release
    )
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 63970L)
    expect_identical(
        object = head(names(object), 3L),
        expected = c("ENSG00000000003", "ENSG00000000005", "ENSG00000000419")
    )
    expect_identical(
        object = lapply(mcols(object), class),
        expected = list(
            broadClass = "factor",
            description = "factor",
            entrezID = "list",
            geneBiotype = "factor",
            geneID = "character",
            geneName = "factor",
            seqCoordSystem = "factor"
        )
    )
})

test_that("makeGRangesFromEnsembl : transcripts", {
    object <- makeGRangesFromEnsembl(
        organism = "Homo sapiens",
        level = "transcripts",
        release = release
    )
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 216741L)
    expect_identical(
        object = head(names(object), 3L),
        expected = c("ENST00000000233", "ENST00000000412", "ENST00000000442")
    )
    expect_identical(
        object = lapply(mcols(object), class),
        expected = list(
            broadClass = "factor",
            description = "factor",
            entrezID = "list",
            geneBiotype = "factor",
            geneID = "factor",
            geneName = "factor",
            seqCoordSystem = "factor",
            transcriptBiotype = "factor",
            transcriptCdsSeqEnd = "integer",
            transcriptCdsSeqStart = "integer",
            transcriptID = "character",
            transcriptName = "character",
            transcriptSupportLevel = "integer"
        )
    )
})

test_that("makeGRangesFromEnsembl : GRCh37", {
    # Genes
    object <- makeGRangesFromEnsembl(
        organism = "Homo sapiens",
        level = "genes",
        build = "GRCh37"
    )
    expect_is(object, "GRanges")
    expect_identical(length(object), 64102L)
    expect_identical(head(names(object), 1L), "ENSG00000000003")

    # Transcripts
    object <- makeGRangesFromEnsembl(
        organism = "Homo sapiens",
        level = "transcripts",
        build = "GRCh37"
    )
    expect_is(object, "GRanges")
    expect_identical(length(object), 215647L)
    expect_identical(head(names(object), 1L), "ENST00000000233")
})

test_that("makeGRangesFromEnsembl : Invalid parameters", {
    expect_error(
        object = makeGRangesFromEnsembl("Homo sapiens", build = "hg38"),
        regexp = "UCSC build ID detected."
    )
    expect_warning(
        object = makeGRangesFromEnsembl("Homo sapiens", release = 86L),
        regexp = "Switching to current release instead."
    )
    expect_error(
        object = makeGRangesFromEnsembl(organism = "AAA", build = "BBB"),
        regexp = "No ID matched on AnnotationHub"
    )
    expect_error(
        object = makeGRangesFromEnsembl(c("Homo sapiens", "Mus musculus")),
        regexp = "is_a_string : "
    )
    expect_error(
        object = makeGRangesFromEnsembl("Homo sapiens", level = "XXX"),
        regexp = "'arg' should be one of \"genes\", \"transcripts\""
    )
})



# makeGRangesFromGFF ===========================================================
test_that("makeGRangesFromGFF : Minimal GTF", {
    # Genes
    object <- makeGRangesFromGFF("example.gtf", level = "genes")
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 17L)
    expect_identical(names(object)[[1L]], "ENSMUSG00000025900")
    expect_identical(
        object = lapply(mcols(object), class),
        expected = list(
            broadClass = "factor",
            geneBiotype = "factor",
            geneID = "character",
            geneName = "character",
            geneSource = "factor",
            geneVersion = "factor",
            source = "factor",
            type = "factor"
        )
    )

    # Transcripts
    object <- makeGRangesFromGFF("example.gtf", level = "transcripts")
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 20L)
    expect_identical(names(object)[[1L]], "ENSMUST00000070533")
    expect_identical(
        object = lapply(mcols(object), class),
        expected = list(
            broadClass = "factor",
            ccdsID = "factor",
            geneBiotype = "factor",
            geneID = "factor",
            geneName = "factor",
            geneSource = "factor",
            geneVersion = "factor",
            source = "factor",
            tag = "factor",
            transcriptBiotype = "factor",
            transcriptID = "character",
            transcriptName = "character",
            transcriptSource = "factor",
            transcriptSupportLevel = "factor",
            transcriptVersion = "factor",
            type = "factor"
        )
    )
})

test_that("makeGRangesFromGFF : Minimal GFF3", {
    # Genes
    object <- makeGRangesFromGFF("example.gff3", level = "genes")
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 20L)
    expect_identical(names(object)[[1L]], "ENSMUSG00000025900")
    expect_identical(
        object = lapply(mcols(object), class),
        expected = list(
            broadClass = "factor",
            description = "character",
            geneBiotype = "factor",
            geneID = "character",
            geneName = "character",
            havanaGene = "factor",
            havanaVersion = "factor",
            logicName = "factor",
            source = "factor",
            type = "factor",
            version = "factor"
        )
    )

    # Transcripts
    object <- makeGRangesFromGFF("example.gff3", level = "transcripts")
    expect_s4_class(object, "GRanges")
    expect_identical(length(object), 26L)
    expect_identical(names(object)[[1L]], "ENSMUST00000027032")
    expect_identical(
        object = lapply(mcols(object), class),
        expected = list(
            broadClass = "factor",
            ccdsID = "factor",
            geneBiotype = "factor",
            geneID = "factor",
            geneName = "factor",
            havanaTranscript = "factor",
            havanaVersion = "factor",
            source = "factor",
            tag = "factor",
            transcriptBiotype = "factor",
            transcriptID = "character",
            transcriptName = "character",
            transcriptSupportLevel = "factor",
            type = "factor",
            version = "factor"
        )
    )
})



# makeTx2geneFromEnsembl =======================================================
test_that("makeTx2geneFromEnsembl", {
    object <- makeTx2geneFromEnsembl(
        organism = "Homo sapiens",
        release = release
    )
    expect_is(object, "tx2gene")
    expect_identical(nrow(object), 216741L)
})



# makeTx2geneFromGFF ===========================================================
with_parameters_test_that(
    "makeTx2geneFromGFF", {
        object <- makeTx2geneFromGFF(file)
        expect_is(object, "tx2gene")
        expect_identical(
            object = head(object, n = 2L),
            expected = new(
                Class = "tx2gene",
                DataFrame(
                    transcriptID = c(
                        "ENSMUST00000193812",
                        "ENSMUST00000082908"
                    ),
                    geneID = c(
                        "ENSMUSG00000102693",
                        "ENSMUSG00000064842"
                    ),
                    row.names = c(
                        "ENSMUST00000193812",
                        "ENSMUST00000082908"
                    )
                )
            )
        )
    },
    file = c("example.gtf", "example.gff3")
)



# organism =====================================================================
with_parameters_test_that(
    "organism : Homo sapiens", {
        expect_identical(organism(object), "Homo sapiens")
    },
    object = c(
        "Homo sapiens",
        "hsapiens",
        "GRCh38",
        "hg38",
        "ENSG00000000001",
        "ENST00000000001"
    )
)

with_parameters_test_that(
    "organism : Mus musculus", {
        expect_identical(organism(object), "Mus musculus")
    },
    object = c(
        "Mus musculus",
        "mmusculus",
        "GRCm38",
        "mm10",
        "ENSMUSG00000000001",
        "ENSMUST00000000001"
    )
)

with_parameters_test_that(
    "organism : Rattus norvegicus", {
        expect_identical(organism(object), "Rattus norvegicus")
    },
    object = c(
        "Rattus norvegicus",
        "rnorvegicus",
        "Rnor_6.0",
        "rn5",
        "ENSRNOG00000000001",
        "ENSRNOT00000000001"
    )
)

with_parameters_test_that(
    "organism : Danio rerio", {
        expect_identical(organism(object), "Danio rerio")
    },
    object = c(
        "Danio rerio",
        "drerio",
        "GRCz10",
        "danRer10",
        "ENSDARG00000000001",
        "ENSDART00000000001"
    )
)

with_parameters_test_that(
    "organism : Drosophila melanogaster", {
        expect_identical(organism(object), "Drosophila melanogaster")
    },
    object = c(
        "Drosophila melanogaster",
        "dmelanogaster",
        "BDGP6",
        "dm6",
        "FBgn0000001",
        "FBtr0000001"
    )
)

with_parameters_test_that(
    "organism : Caenorhabditis elegans", {
        expect_identical(organism(object), "Caenorhabditis elegans")
    },
    object = c(
        "Caenorhabditis elegans",
        "celegans",
        "WBcel235",
        "ce11",
        "WBGene00000001"
    )
)

with_parameters_test_that(
    "organism : Gallus gallus", {
        expect_identical(organism(object), "Gallus gallus")
    },
    object = c(
        "Gallus gallus",
        "ggallus",
        "ENSGALG00000000001",
        "ENSGALT00000000001"
    )
)

with_parameters_test_that(
    "organism : Ovis aries", {
        expect_identical(organism(object), "Ovis aries")
    },
    object = c(
        "Ovis aries",
        "oaries",
        "Oar_v3.1",
        "oviAri3",
        "ENSOARG00000000001",
        "ENSOART00000000001"
    )
)

test_that("organism : Multiple organisms", {
    # Function matches only the first genome.
    expect_identical(
        object = organism(c(
            "ENSG00000000001",
            "ENSG00000000002",
            "ENSMUSG00000000001",
            "ENSMUSG00000000002"
        )),
        expected = "Homo sapiens"
    )
})

test_that("organism : matrix", {
    object <- mat
    expect_identical(
        object = organism(object),
        expected = "Homo sapiens"
    )
})

test_that("organism : Detection failure", {
    expect_error(
        object = organism("XXX"),
        regexp = "Failed to detect organism"
    )
})



# panther ======================================================================
with_parameters_test_that(
    "panther", {
        invisible(capture.output(
            object <- panther(organism)
        ))
        expect_is(object, "DataFrame")
    },
    organism = names(.pantherMappings)
)



# stripTranscriptVersions ======================================================
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
