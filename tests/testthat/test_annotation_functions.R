organism <- "Homo sapiens"
release <- 87L



# General ======================================================================
context("Annotation Functions : General")

with_parameters_test_that(
    "convertUCSCBuildToEnsembl", {
        expect_identical(
            object = convertUCSCBuildToEnsembl(object) %>%
                as.character(),
            expected = expected
        )
    },
    object = list(
        "hg19",
        "hg38",
        "mm10"
    ),
    expected = list(
        "GRCh37",
        "GRCh38",
        "GRCm38"
    )
)

test_that("convertUCSCBuildToEnsembl : Failure", {
    expect_error(
        object = convertUCSCBuildToEnsembl("XXX"),
        regexp = "Failed to match UCSC"
    )
})

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

    # mcolsNames argument.
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



# Organism Matching ============================================================
context("Annotation Functions : Organism Matching")

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

test_that("organism : Failure", {
    expect_error(
        object = organism("XXX"),
        regexp = "Failed to detect organism"
    )
})



# AnnotationHub ================================================================
context("Annotation Functions : AnnotationHub")

test_that("makeGRangesFromEnsembl : genes", {
    object <- makeGRangesFromEnsembl(
        organism = organism,
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
        organism = organism,
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

test_that("annotable", {
    object <- annotable(organism, release = release)
    expect_is(object, "tbl_df")
    expect_identical(dim(object), c(63970L, 12L))
    expect_identical(object[["geneID"]][[1L]], "ENSG00000000003")
})

gene2symbol <- makeGene2symbolFromEnsembl(
    organism = organism,
    release = release
)

test_that("makeGene2symbolFromEnsembl", {
    expect_is(gene2symbol, "gene2symbol")
    expect_identical(nrow(gene2symbol), 63970L)
})

tx2gene <- makeTx2geneFromEnsembl(
    organism = organism,
    release = release
)

test_that("makeTx2geneFromEnsembl", {
    expect_is(tx2gene, "tx2gene")
    expect_identical(nrow(tx2gene), 216741L)
})



# GTF/GFF ======================================================================
context("Annotation Functions : GTF/GFF")

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



# Remapping ====================================================================
context("Annotation Functions : Remapping")

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

test_that("convertGenesToSymbols : FASTA spike-in support", {
    # Specify organism (to handle FASTA spike-ins (e.g. EGFP).
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
        convertGenesToSymbols("ENSG00000000000", gene2symbol = gene2symbol),
        "Failed to match genes: ENSG00000000000"
    )
    expect_error(
        convertGenesToSymbols(c("ENSG00000000003", NA)),
        "is_non_missing_nor_empty_character :"
    )
    expect_error(
        convertGenesToSymbols(c("ENSG00000000003", "")),
        "is_non_missing_nor_empty_character :"
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
        regexp = "is_non_missing_nor_empty_character :"
    )
    expect_error(
        object = convertTranscriptsToGenes(c("ENST00000000233", "")),
        regexp = "is_non_missing_nor_empty_character :"
    )
})



# Databases ====================================================================
context("Annotation Functions : Databases")

# FIXME Add code coverage:
# - ensembl2entrez
# - geneSynonyms
# - hgnc2ensembl
# - mgi2ensembl

test_that("eggnog", {
    object <- eggnog(.test = TRUE)
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

with_parameters_test_that(
    "geneSynonyms", {
        object <- geneSynonyms(organism = organism, .test = TRUE)
        expect_is(object, "grouped_df")
    },
    organism = .geneSynonymsOrganisms
)

with_parameters_test_that(
    "panther", {
        invisible(capture.output(
            object <- panther(organism, .test = TRUE)
        ))
        expect_is(object, "DataFrame")
    },
    organism = names(.pantherMappings)
)
