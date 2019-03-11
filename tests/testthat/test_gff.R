context("GFF file import")

levels <- c("genes", "transcripts")



# Ensembl ======================================================================
with_parameters_test_that(
    "Ensembl GFF", {
        file <- "ensembl.gff3.gz"
        object <- makeGRangesFromGFF(file = file, level = level)
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(53L, 147L)
)

with_parameters_test_that(
    "Ensembl GTF", {
        file <- "ensembl.gtf.gz"
        object <- makeGRangesFromGFF(file = file, level = level)
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(60L, 167L)
)



# FlyBase ======================================================================
with_parameters_test_that(
    "FlyBase GTF", {
        file <- "flybase.gtf.gz"
        expect_warning(
            makeGRangesFromGFF(file, level = level),
            "GRanges does not contain biotype in mcols\\(\\)."
        )
        suppressWarnings(
            object <- makeGRangesFromGFF(file, level = level)
        )
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(39L, 72L)
)



# GENCODE ======================================================================
with_parameters_test_that(
    "GENCODE GFF", {
        file <- "gencode.gff3.gz"
        object <- makeGRangesFromGFF(file, level = level)
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(60L, 167L)
)

with_parameters_test_that(
    "GENCODE GTF", {
        file <- "gencode.gtf.gz"
        object <- makeGRangesFromGFF(file, level = level)
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(60L, 167L)
)



# RefSeq =======================================================================
test_that("RefSeq GFF", {
    file <- "refseq.gff.gz"

    # Genes
    object <- makeGRangesFromGFF(file = file, level = "genes")
    expect_s4_class(object, "GRanges")
    expect_length(object, 62L)

    # Transcripts
    object <- makeGRangesFromGFF(file = file, level = "transcripts")
    expect_s4_class(object, "GRangesList")
    expect_length(object, 100L)
})



# WormBase =====================================================================
with_parameters_test_that(
    "WormBase GTF", {
        file <- "wormbase.gtf.gz"
        object <- makeGRangesFromGFF(file = file, level = level)
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(66L, 83L)
)
