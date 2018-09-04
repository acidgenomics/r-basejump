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
        SummarizedExperiment = rse_small
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
        expected = c(
            "ENSMUSG00000000001" = "Gnai3",
            "ENSMUSG00000000003" = "Pbsn"
        )
    )
})

test_that("convertGenesToSymbols : FASTA spike-in support", {
    # Specify organism (to handle FASTA spike-ins (e.g. EGFP)
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
    expected <- c(
        "ENSMUST00000000001" = "ENSMUSG00000000001",
        "ENSMUST00000000003" = "ENSMUSG00000000003"
    )

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
        expected = c(
            "ENSMUST00000000001" = "ENSMUSG00000000001",
            "ENSMUST00000000003" = "ENSMUSG00000000003",
            "ENSMUST00000114041" = "ENSMUSG00000000003"
        )
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



# detectOrganism ===============================================================
test_that("detectOrganism : Homo sapiens", {
    object <- "Homo sapiens"
    expect_identical(object, detectOrganism("Homo sapiens"))
    expect_identical(object, detectOrganism("hsapiens"))
    expect_identical(object, detectOrganism("GRCh38"))
    expect_identical(object, detectOrganism("grch38"))
    expect_identical(object, detectOrganism("hg38"))
    expect_identical(object, detectOrganism("ENSG00000000001"))
    expect_identical(object, detectOrganism("ENST00000000001"))
})

test_that("detectOrganism : Mus musculus", {
    object <- "Mus musculus"
    expect_identical(object, detectOrganism("Mus musculus"))
    expect_identical(object, detectOrganism("mmusculus"))
    expect_identical(object, detectOrganism("GRCm38"))
    expect_identical(object, detectOrganism("grcm38"))
    expect_identical(object, detectOrganism("mm10"))
    expect_identical(object, detectOrganism("ENSMUSG00000000001"))
    expect_identical(object, detectOrganism("ENSMUST00000000001"))
})

test_that("detectOrganism : Rattus norvegicus", {
    object <- "Rattus norvegicus"
    expect_identical(object, detectOrganism("Rattus norvegicus"))
    expect_identical(object, detectOrganism("rnorvegicus"))
    expect_identical(object, detectOrganism("ENSRNOG00000000001"))
    expect_identical(object, detectOrganism("ENSRNOT00000000001"))
})

test_that("detectOrganism : Danio rerio", {
    object <- "Danio rerio"
    expect_identical(object, detectOrganism("Danio rerio"))
    expect_identical(object, detectOrganism("drerio"))
    expect_identical(object, detectOrganism("GRCz10"))
    expect_identical(object, detectOrganism("danRer10"))
    expect_identical(object, detectOrganism("ENSDARG00000000001"))
    expect_identical(object, detectOrganism("ENSDART00000000001"))
})

test_that("detectOrganism : Drosophila melanogaster", {
    object <- "Drosophila melanogaster"
    expect_identical(object, detectOrganism("Drosophila melanogaster"))
    expect_identical(object, detectOrganism("dmelanogaster"))
    expect_identical(object, detectOrganism("BDGP6"))
    expect_identical(object, detectOrganism("dm6"))
    expect_identical(object, detectOrganism("FBgn0000001"))
    expect_identical(object, detectOrganism("FBtr0000001"))
})

test_that("detectOrganism : Caenorhabditis elegans", {
    object <- "Caenorhabditis elegans"
    expect_identical(object, detectOrganism("Caenorhabditis elegans"))
    expect_identical(object, detectOrganism("celegans"))
    expect_identical(object, detectOrganism("WBcel235"))
    expect_identical(object, detectOrganism("ce11"))
    expect_identical(object, detectOrganism("WBGene00000001"))
})

test_that("detectOrganism : Gallus gallus", {
    object <- "Gallus gallus"
    expect_identical(object, detectOrganism("Gallus gallus"))
    expect_identical(object, detectOrganism("ggallus"))
    expect_identical(object, detectOrganism("ENSGALG00000000001"))
    expect_identical(object, detectOrganism("ENSGALT00000000001"))
})

test_that("detectOrganism : Ovis aries", {
    object <- "Ovis aries"
    expect_identical(object, detectOrganism("Ovis aries"))
    expect_identical(object, detectOrganism("oaries"))
    expect_identical(object, detectOrganism("ENSOARG00000000001"))
    expect_identical(object, detectOrganism("ENSOART00000000001"))
})

test_that("detectOrganism : Multiple organisms", {
    object <- c(
        "ENSG00000000001",
        "ENSG00000000002",
        "ENSMUSG00000000001",
        "ENSMUSG00000000002"
    )
    expect_identical(
        object = suppressWarnings(detectOrganism(object, unique = FALSE)),
        expected = c(
            "ENSG00000000001" = "Homo sapiens",
            "ENSG00000000002" = "Homo sapiens",
            "ENSMUSG00000000001" = "Mus musculus",
            "ENSMUSG00000000002" = "Mus musculus"
        )
    )
    expect_identical(
        object = suppressWarnings(detectOrganism(object, unique = TRUE)),
        expected = c("Homo sapiens", "Mus musculus")
    )
    expect_warning(
        object = detectOrganism(object),
        regexp = "Multiple organisms detected"
    )
})

test_that("detectOrganism : Detection failure", {
    expect_error(
        object = detectOrganism("XXX"),
        regexp = "Failed to detect organism"
    )
})

test_that("detectOrganism : matrix", {
    object <- mat
    expect_identical(
        object = detectOrganism(object),
        expected = "Homo sapiens"
    )
})

test_that("detectOrganism : tbl_df", {
    object <- as(object, "tbl_df")
    expect_true("rowname" %in% colnames(object))
    expect_identical(
        detectOrganism(object),
        "Homo sapiens"
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
    expect_identical(colnames(object), c("geneID", "geneName"))
    expect_identical(nrow(object), 63970L)
})



# makeGene2symbolFromGFF =======================================================
test_that("makeGene2symbolFromGFF : Minimal GTF", {
    object <- makeGene2symbolFromGFF("example.gtf")
    expect_is(object, "DataFrame")
    expect_identical(dim(object), c(17L, 2L))
    expect_identical(
        object = head(object, 2L),
        expected = DataFrame(
            geneID = c("ENSMUSG00000025900", "ENSMUSG00000051951"),
            geneName = c("Rp1", "Xkr4"),
            row.names = c("ENSMUSG00000025900", "ENSMUSG00000051951")
        )
    )
})

test_that("makeGene2symbolFromGFF : Minimal GFF3", {
    object <- makeGene2symbolFromGFF("example.gff3")
    expect_is(object, "DataFrame")
    expect_identical(dim(object), c(20L, 2L))
    expect_identical(
        object = head(object, 2L),
        expected = DataFrame(
            geneID = c("ENSMUSG00000025900", "ENSMUSG00000025902"),
            geneName = c("Rp1", "Sox17"),
            row.names = c("ENSMUSG00000025900", "ENSMUSG00000025902")
        )
    )
})



# makeGRangesFromEnsembl =======================================================
test_that("makeGRangesFromEnsembl : genes", {
    object <- makeGRangesFromEnsembl(
        organism = "Homo sapiens",
        format = "genes",
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
        format = "transcripts",
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
        format = "genes",
        build = "GRCh37"
    )
    expect_is(object, "GRanges")
    expect_identical(length(object), 64102L)
    expect_identical(head(names(object), 1L), "ENSG00000000003")

    # Transcripts
    object <- makeGRangesFromEnsembl(
        organism = "Homo sapiens",
        format = "transcripts",
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
        object = makeGRangesFromEnsembl("Homo sapiens", format = "XXX"),
        regexp = "'arg' should be one of \"genes\", \"transcripts\""
    )
})

test_that("makeGRangesFromEnsembl : metadata", {
    object <- makeGRangesFromEnsembl(
        organism = "Homo sapiens",
        release = release,
        metadata = TRUE
    )
    expect_is(object, "list")
    expect_identical(
        object = names(object),
        expected = c("data", "metadata")
    )
    expect_is(object[["metadata"]], "tbl_df")
})



# makeGRangesFromGFF ===========================================================
test_that("makeGRangesFromGFF : Minimal GTF", {
    # Genes
    object <- makeGRangesFromGFF("example.gtf", format = "genes")
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
    object <- makeGRangesFromGFF("example.gtf", format = "transcripts")
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
    object <- makeGRangesFromGFF("example.gff3", format = "genes")
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
    object <- makeGRangesFromGFF("example.gff3", format = "transcripts")
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
    expect_identical(colnames(object), c("transcriptID", "geneID"))
    expect_identical(nrow(object), 216741L)
})



# makeTx2geneFromGFF ===========================================================
test_that("makeTx2geneFromGFF : Minimal GTF", {
    object <- makeTx2geneFromGFF("example.gtf")
    expect_is(object, "DataFrame")
    expect_identical(dim(object), c(20L, 2L))
    expect_identical(
        object = head(object, 2L),
        expected = DataFrame(
            transcriptID = c("ENSMUST00000070533", "ENSMUST00000082908"),
            geneID = c("ENSMUSG00000051951", "ENSMUSG00000064842"),
            row.names = c("ENSMUST00000070533", "ENSMUST00000082908")
        )
    )
})

test_that("makeTx2geneFromGFF : Minimal GFF3", {
    object <- makeTx2geneFromGFF("example.gff3")
    expect_is(object, "DataFrame")
    expect_identical(dim(object), c(26L, 2L))
    expect_identical(
        object = head(object, 2L),
        expected = DataFrame(
            transcriptID = c("ENSMUST00000027032", "ENSMUST00000027035"),
            geneID = c("ENSMUSG00000025900", "ENSMUSG00000025902"),
            row.names = c("ENSMUST00000027032", "ENSMUST00000027035")
        )
    )
})



# panther ======================================================================
with_parameters_test_that(
    "panther", {
        invisible(capture.output(
            object <- panther(organism)
        ))
        expect_is(object, "data.frame")
    },
    organism = names(.pantherMappings)
)



# stripTranscriptVersions ======================================================
test_that("stripTranscriptVersions : character", {
    expect_identical(
        object = stripTranscriptVersions("ENSMUST00000119854.7"),
        expected = "ENSMUST00000119854"
    )
    # Return unmodified if not Ensembl transcript (ENS*T).
    expect_identical(
        object = stripTranscriptVersions("EGFP.1"),
        expected = "EGFP.1"
    )
    # Theoretical spike-in containing a transcript version.
    expect_identical(
        object = stripTranscriptVersions(c("ENSMUST00000119854.7", "EGFP.1")),
        expected = c("ENSMUST00000119854", "EGFP.1")
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
